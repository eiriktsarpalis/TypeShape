#load "../src/TypeShape/TypeShape.fs"
#r "../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec/lib/net40-client/FParsec.dll"

open System
open FParsec
open TypeShape

let rec mkParser<'T> () : string -> 'T = 
    let fp = mkFParser<'T>() in 
    fun inp -> 
        match run fp inp with
        | Success(r,_,_) -> r
        | Failure(msg,_,_) -> failwithf "Parse error: %s" msg

and private mkFParser<'T> () : Parser<'T,unit> = mkParserUntyped typeof<'T> :?> _
and private mkParserUntyped (t : Type) : obj =
    let wrap (p : Parser<'T,unit>) = box(spaces >>. p .>> spaces)
    match TypeShape.Resolve t with
    | Shape.Unit -> wrap(pstring "(" >>. spaces .>> pstring ")")
    | Shape.Bool -> wrap(stringReturn "true" true <|> stringReturn "false" false)
    | Shape.Byte -> wrap(puint8)
    | Shape.Int32 -> wrap(pint32)
    | Shape.Int64 -> wrap(pint64)
    | Shape.String -> wrap(pchar '\"' >>. manySatisfy ((<>) '\"') .>> pchar '\"')
    | Shape.FSharpOption s ->
        s.Accept {
            new IFSharpOptionVisitor<obj> with
                member __.Visit<'T> () =
                    let tp = mkFParser<'T>() |>> Some
                    let nP = stringReturn "None" None
                    let sP = (pstring "Some" >>. spaces >>. pstring "(" >>. tp .>> pstring ")")
                    wrap(nP <|> sP)
        }

    | Shape.FSharpList s ->
        s.Accept {
            new IFSharpListVisitor<obj> with
                member __.Visit<'T> () =
                    let tp = mkFParser<'T>()
                    let sep = spaces >>. pstring ";" >>. spaces
                    let lp = pstring "[" >>. spaces >>. sepBy tp sep .>> spaces .>> pstring "]"
                    wrap lp
        }

    | Shape.Array s ->
        s.Accept {
            new IArrayVisitor<obj> with
                member __.Visit<'T> () =
                    let tp = mkFParser<'T> ()
                    let sep = spaces >>. pstring ";" >>. spaces
                    let lp = pstring "[|" >>. spaces >>. sepBy tp sep .>> spaces .>> pstring "|]"
                    wrap(lp |>> Array.ofList)
        }

    | Shape.Tuple2 s ->
        s.Accept {
            new ITuple2Visitor<obj> with
                member __.Visit<'T, 'S> () =
                    let tp = spaces >>. mkFParser<'T>() .>> spaces .>> pstring "," .>> spaces
                    let sp = spaces >>. mkFParser<'S>() .>> spaces
                    wrap(pstring "(" >>. tuple2 tp sp .>> pstring ")")
        }

    | Shape.Tuple3 s ->
        s.Accept {
            new ITuple3Visitor<obj> with
                member __.Visit<'T1, 'T2, 'T3> () =
                    let t1p = spaces >>. mkFParser<'T1>() .>> spaces .>> pstring "," .>> spaces
                    let t2p = spaces >>. mkFParser<'T2>() .>> spaces .>> pstring "," .>> spaces
                    let t3p = spaces >>. mkFParser<'T3>() .>> spaces
                    wrap(pstring "(" >>. tuple3 t1p t2p t3p .>> pstring ")")
        }

    | Shape.FSharpRecord1 s ->
        s.Accept {
            new IFSharpRecord1Visitor<obj> with
                member __.Visit (s : IShapeFSharpRecord<'Record, 'Field1>) =
                    let f1p = spaces >>. pstring s.Properties.[0].Name >>. spaces >>. pstring "=" >>. spaces >>. mkFParser<'Field1>() .>> spaces
                    wrap(pstring "{" >>. f1p |>> s.Construct .>> pstring "}")
        }

    | Shape.FSharpRecord2 s ->
        s.Accept {
            new IFSharpRecord2Visitor<obj> with
                member __.Visit<'Record,'Field1,'Field2> (s : IShapeFSharpRecord<'Record,'Field1,'Field2>) =
                    let f1p = spaces >>. pstring s.Properties.[0].Name >>. spaces >>. pstring "=" >>. spaces >>. mkFParser<'Field1>() .>> spaces .>> pstring ";" .>> spaces
                    let f2p = spaces >>. pstring s.Properties.[1].Name >>. spaces >>. pstring "=" >>. spaces >>. mkFParser<'Field2>() .>> spaces
                    wrap(pstring "{" >>. tuple2 f1p f2p |>> s.Construct .>> pstring "}")
        }

    | Shape.FSharpUnion1 s ->
        s.Accept {
            new IFSharpUnion1Visitor<obj> with
                member __.Visit (s : IShapeFSharpUnion<'Union,'Case1>) =
                    let c1p = mkFParser<'Case1>() |>> s.Construct1
                    let u1p = pstring s.UnionCaseInfo.[0].Name >>. spaces >>. pstring "(" >>. c1p .>> pstring ")"
                    wrap c1p
        }

    | Shape.FSharpUnion2 s ->
        s.Accept {
            new IFSharpUnion2Visitor<obj> with
                member __.Visit<'Union,'Case1,'Case2> (s : IShapeFSharpUnion<'Union,'Case1,'Case2>) =
                    let c1p, c2p = mkFParser<'Case1>() |>> s.Construct1, mkFParser<'Case2>() |>> s.Construct2
                    let u1p = pstring s.UnionCaseInfo.[0].Name >>. spaces >>. pstring "(" >>. c1p .>> pstring ")"
                    let u2p = pstring s.UnionCaseInfo.[1].Name >>. spaces >>. pstring "(" >>. c2p .>> pstring ")"
                    wrap(u1p <|> u2p)
        }

    | _ -> failwithf "unsupported type '%O'" t


let p1 = mkParser<int * int list>()
p1 "(42, [1;2;3])"

let p2 = mkParser<int * string list option * string ref>()
p2 """(42, Some (["1" ;  "2"]), { contents= "value" }) ) """