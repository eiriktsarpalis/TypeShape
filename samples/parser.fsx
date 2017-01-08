#r "../bin/TypeShape.dll"
#r "../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec/lib/net40-client/FParsec.dll"

open System
open FParsec
open TypeShape
open TypeShape_Utils

type Parser<'T> = Parser<'T, unit>

let inline delay (f : unit -> 'T) : Parser<'T> =
    fun _ -> Reply(f())

let spaced p = between spaces spaces p

let (<*>) (f : Parser<'T -> 'S>) (t : Parser<'T>) : Parser<'S> = 
    parse {
        let! tv = t
        let! fv = f
        return fv tv
    }

/// Generates a parser for supplied type
let rec genParser<'T> () : Parser<'T> =
    match cache.TryFind<Parser<'T>> () with
    | Some p -> p
    | None ->
        // create a delayed uninitialized instance for recursive type definitions
        let _ = cache.CreateUninitialized<Parser<'T>>(fun c s -> c.Value s)
        let p = genParserAux<'T> ()
        cache.Commit (spaced p)
    
and genParserAux<'T> () : Parser<'T> =
    let token str = spaced (pstring str) >>% ()
    let paren p = between (pchar '(') (pchar ')') (spaced p)
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
                    let sep = pchar ';'
                    let lp = between (pchar '[') (pchar ']') (sepBy tp sep)
                    wrap lp
        }

    | Shape.Array s when s.Rank = 1 ->
        s.Accept {
            new IArrayVisitor<Parser<'T>> with
                member __.Visit<'t> _ =
                    let tp = genParser<'t> ()
                    let sep = pchar ';'
                    let lp = between (pstring "[|") (pstring "|]") (sepBy tp sep)
                    wrap(lp |>> Array.ofList)
        }

    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
        let init = delay shape.CreateUninitialized
        let eps = shape.Elements |> Array.map mkMemberParser
        let composed = combineMemberParsers init eps (pchar ',')
        paren composed

    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
        let init = delay shape.CreateUninitialized
        let fps = 
            shape.Fields 
            |> Array.map (fun f -> token f.Label >>. pchar '=' >>. mkMemberParser f)

        let composed = combineMemberParsers init fps (pchar ';')
        between (pchar '{') (pchar '}') composed

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
        let mkUnionCaseParser (case : ShapeFSharpUnionCase<'T>) =
            let caseName = pstring case.CaseInfo.Name
            let init = delay case.CreateUninitialized
            match case.Fields |> Array.map mkMemberParser with
            | [||] -> caseName >>. init
            | fps ->
                let composed = combineMemberParsers init fps (pchar ',')
                let valueP = 
                    if fps.Length = 1 then paren composed <|> composed
                    else paren composed

                caseName >>. spaces >>. valueP

        shape.UnionCases
        |> Array.map mkUnionCaseParser
        |> choice

    | _ -> failwithf "unsupported type '%O'" typeof<'T>
 
and private cache : TypeCache = new TypeCache()


/// Generates a string parser for given type
let mkParser<'T> () : string -> 'T = 
    let fp = genParser<'T>() .>> eof
    fun inp -> 
        match run fp inp with
        | Success(r,_,_) -> r
        | Failure(msg,_,_) -> failwithf "Parse error: %s" msg

//--------------------------
// Examples

let p1 = mkParser<int * int list>()
p1 "(42, [1;2;3])"

let p2 = mkParser<int * string list option * string ref>()
p2 """(42, Some (["1" ;  "2"]), { contents= "value" } ) """

type Foo = { A : int ; B : string }

type Bar =
    | Foo of Foo
    | Bar of int
    | C
    | D of string option

let p3 = mkParser<Bar list []>()

p3 """ [| [ Bar 42 ; Bar(42) ; Foo { A = 12 ; B = "Foo" } ; C ] ; [] ; [D (Some "42")]|] """

// Recursive type parsing

type BinTree<'T> = Leaf | Node of 'T * BinTree<'T> * BinTree<'T>

let p4 = mkParser<BinTree<int>> ()

p4 "Node(3, Node(1, Leaf, Node(2, Leaf,Leaf)), Leaf)"