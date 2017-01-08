#r "../bin/TypeShape.dll"
#r "../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec/lib/net40-client/FParsec.dll"

open System
open FParsec
open TypeShape

type Parser<'T> = Parser<'T, unit>

let inline delay (f : unit -> 'T) : Parser<'T> =
    fun _ -> Reply(f())

let (<*>) (f : Parser<'T -> 'S>) (t : Parser<'T>) : Parser<'S> = 
    parse {
        let! tv = t
        let! fv = f
        return fv tv
    }

/// Generates a parser for supplied type
let rec genParser<'T> () : Parser<'T> =
    let spaced p = between spaces spaces p
    let token str = spaced (pstring str) >>% ()
    let paren p = between (token "(") (token ")") p
    let wrap (p : Parser<'a>) = unbox<Parser<'T>>(spaced p)

    let mkMemberParser (shape : IShapeWriteMember<'Class>) =
        shape.Accept { new IWriteMemberVisitor<'Class, Parser<'Class -> 'Class>> with
            member __.Visit (shape : ShapeWriteMember<'Class, 'Field>) =
                let fp = genParser<'Field>()
                fp |>> fun f dt -> shape.Inject dt f
        }

    let combineMemberParsers 
        (init : Parser<'Class>)
        (injectors : Parser<'Class -> 'Class> [])
        (separator : Parser<'Sep>) =

        match Array.toList injectors with
        | [] -> init
        | hd :: tl -> List.fold (fun acc i -> (separator >>. i) <*> acc) (hd <*> init) tl

    match shapeof<'T> with
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
                    let tp = genParser<'t>() |>> Some
                    let nP = stringReturn "None" None
                    let vp = attempt (paren tp) <|> tp
                    let sP = token "Some" >>. vp
                    wrap(nP <|> sP)
        }

    | Shape.FSharpList s ->
        s.Accept {
            new IFSharpListVisitor<Parser<'T>> with
                member __.Visit<'t> () =
                    let tp = genParser<'t>()
                    let sep = token ";"
                    let lp = between (token "[") (token "]") (sepBy tp sep)
                    wrap lp
        }

    | Shape.Array s when s.Rank = 1 ->
        s.Accept {
            new IArrayVisitor<Parser<'T>> with
                member __.Visit<'t> _ =
                    let tp = genParser<'t> ()
                    let sep = token ";"
                    let lp = between (token "[|") (token "|]") (sepBy tp sep)
                    wrap(lp |>> Array.ofList)
        }

    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
        let init = delay shape.CreateUninitialized
        let eps = shape.Elements |> Array.map mkMemberParser
        let composed = combineMemberParsers init eps (token ",")
        paren composed

    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
        let init = delay shape.CreateUninitialized
        let fps = 
            shape.Fields 
            |> Array.map (fun f -> token f.Label >>. token "=" >>. mkMemberParser f)

        let composed = combineMemberParsers init fps (token ";")
        between (token "{") (token "}") composed

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
        let mkUnionCaseParser (case : ShapeFSharpUnionCase<'T>) =
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

        shape.UnionCases 
        |> Array.map mkUnionCaseParser
        |> choice

    | _ -> failwithf "unsupported type '%O'" typeof<'T>


/// Generates a string parser for given type
let mkParser<'T> () : string -> 'T = 
    let fp = genParser<'T>() in 
    fun inp -> 
        match run fp inp with
        | Success(r,_,_) -> r
        | Failure(msg,_,_) -> failwithf "Parse error: %s" msg

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

p3 """ [| [ Bar 42 ; Bar(42) ; Foo { A = 12 ; B = "Foo" } ; C] ; [] ; [D (Some "42")]|] """