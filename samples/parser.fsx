#r "../bin/TypeShape.dll"
#r "../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec/lib/net40-client/FParsec.dll"

open System
open FParsec
open TypeShape

[<AutoOpen>]
module Utils =
    type Parser<'T> = Parser<'T, unit>

    let inline delay (f : unit -> 'T) : Parser<'T> =
        fun stream -> Reply(f ())

    /// Parser folding combinator with separator support
    let fold (folder : 'State -> 'T -> 'State) 
                (init : Parser<'State>) (parsers : Parser<'T> []) 
                (sep : Parser<'Sep>) : Parser<'State> =

        fun stream ->
            let mutable state = init stream
            if state.Status <> Ok then state else
            let mutable success = true
            let mutable i = 0
            let n = parsers.Length
            while i < n && success do
                if i > 0 then 
                    let r = sep stream 
                    if r.Status <> Ok then
                        success <- false
                        state <- Reply(r.Status, r.Error)

                if success then
                    let r = parsers.[i] stream
                    if r.Status <> Ok then
                        success <- false
                        state <- Reply(r.Status, r.Error)
                    else
                        state <- Reply(folder state.Result r.Result)

                i <- i + 1

            state


/// Generates a parser for given type
let rec mkParser<'T> () : string -> 'T = 
    let fp = mkFParser<'T>() in 
    fun inp -> 
        match run fp inp with
        | Success(r,_,_) -> r
        | Failure(msg,_,_) -> failwithf "Parse error: %s" msg

and private mkFParser<'T> () : Parser<'T> =
    let spaced p = between spaces spaces p
    let token str = spaced (pstring str) >>% ()
    let paren p = between (token "(") (token ")") p
    let wrap (p : Parser<'a>) = unbox<Parser<'T>>(spaced p)

    let mkMemberParser (shape : IShapeWriteMember<'DeclaringType>) =
        shape.Accept { new IWriteMemberVisitor<'DeclaringType, Parser<'DeclaringType -> 'DeclaringType>> with
            member __.Visit (shape : ShapeWriteMember<'DeclaringType, 'Field>) =
                let fp = mkFParser<'Field>()
                fp |>> fun f dt -> shape.Inject dt f
        }

    let combineMemberParsers 
        (init : Parser<'DeclaringType>) 
        (injectors : Parser<'DeclaringType -> 'DeclaringType> [])
        (separator : Parser<'Sep>) =

        fold (fun d i -> i d) init injectors separator

    match TypeShape.Create<'T>() with
    | Shape.Unit -> wrap(paren spaces)
    | Shape.Bool -> wrap(stringReturn "true" true <|> stringReturn "false" false)
    | Shape.Byte -> wrap(puint8)
    | Shape.Int32 -> wrap(pint32)
    | Shape.Int64 -> wrap(pint64)
    | Shape.String -> wrap(between (pchar '\"') (pchar '\"') (manySatisfy ((<>) '\"')))
    | Shape.FSharpOption s ->
        s.Accept {
            new IFSharpOptionVisitor<Parser<'T>> with
                member __.Visit<'t> () =
                    let tp = mkFParser<'t>() |>> Some
                    let nP = stringReturn "None" None
                    let vp = attempt (paren tp) <|> tp
                    let sP = token "Some" >>. vp
                    wrap(nP <|> sP)
        }

    | Shape.FSharpList s ->
        s.Accept {
            new IFSharpListVisitor<Parser<'T>> with
                member __.Visit<'t> () =
                    let tp = mkFParser<'t>()
                    let sep = token ";"
                    let lp = between (token "[") (token "]") (sepBy tp sep)
                    wrap lp
        }

    | Shape.Array s when s.Rank = 1 ->
        s.Accept {
            new IArrayVisitor<Parser<'T>> with
                member __.Visit<'t> _ =
                    let tp = mkFParser<'t> ()
                    let sep = token ";"
                    let lp = between (token "[|") (token "|]") (sepBy tp sep)
                    wrap(lp |>> Array.ofList)
        }

    | Shape.Tuple s ->
        s.Accept { new ITupleVisitor<Parser<'T>> with
            member __.Visit (shape : ShapeTuple<'Tuple>) =
                let init = delay shape.CreateUninitialized
                let eps = shape.Elements |> Array.map mkMemberParser
                let composed = combineMemberParsers init eps (token ",")
                wrap(paren composed) }

    | Shape.FSharpRecord s ->
        s.Accept { new IFSharpRecordVisitor<Parser<'T>> with
            member __.Visit (shape : ShapeFSharpRecord<'Record>) =
                let init = delay shape.CreateUninitialized
                let fps = 
                    shape.Fields 
                    |> Array.map (fun f -> token f.Label >>. token "=" >>. mkMemberParser f)

                let composed = combineMemberParsers init fps (token ";")
                wrap(between (token "{") (token "}") composed) }

    | Shape.FSharpUnion s ->
        s.Accept { new IFSharpUnionVisitor<Parser<'T>> with
            member __.Visit (shape : ShapeFSharpUnion<'Union>) =
                let mkUnionCaseParser (case : ShapeFSharpUnionCase<'Union>) =
                    let caseName = pstring case.CaseInfo.Name
                    let init = delay case.CreateUninitialized
                    match case.Fields |> Array.map mkMemberParser with
                    | [||] -> caseName >>. init
                    | fps ->
                        let composed = combineMemberParsers init fps (token ",")
                        let parenP = paren composed
                        let valueP = 
                            if fps.Length = 1 then attempt parenP <|> composed
                            else parenP

                        caseName >>. valueP

                let unionParser =
                    shape.UnionCases 
                    |> Array.map mkUnionCaseParser
                    |> choice

                wrap unionParser }

    | _ -> failwithf "unsupported type '%O'" typeof<'T>


// examples

let p1 = mkParser<int * int list>()
p1 "(42, [1;2;3])"

let p2 = mkParser<int * string list option * string ref>()
p2 """(42, Some (["1" ;  "2"]), { contents= "value" }) ) """

type Foo = { A : int ; B : string }

type Bar =
    | Foo of Foo
    | Bar of int
    | C
    | D of string option

let p3 = mkParser<Bar list []>()

p3 """ [| [ Bar 42 ; Bar(42) ; Foo({ A = 12 ; B = "Foof" }) ; C ] ; [] ; [D (Some "42")]|] """