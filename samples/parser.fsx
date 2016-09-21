#r "../bin/TypeShape.dll"
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

and private mkFParser<'T> () : Parser<'T,unit> =
    let wrap (p : Parser<'a,unit>) = unbox<Parser<'T,unit>>(spaces >>. p .>> spaces)
    match TypeShape.Create<'T>() with
    | Shape.Unit -> wrap(pstring "(" >>. spaces .>> pstring ")")
    | Shape.Bool -> wrap(stringReturn "true" true <|> stringReturn "false" false)
    | Shape.Byte -> wrap(puint8)
    | Shape.Int32 -> wrap(pint32)
    | Shape.Int64 -> wrap(pint64)
    | Shape.String -> wrap(pchar '\"' >>. manySatisfy ((<>) '\"') .>> pchar '\"')
    | Shape.FSharpOption s ->
        s.Accept {
            new IFSharpOptionVisitor<Parser<'T,unit>> with
                member __.Visit<'t> () =
                    let tp = mkFParser<'t>() |>> Some
                    let nP = stringReturn "None" None
                    let sP = (pstring "Some" >>. spaces >>. pstring "(" >>. tp .>> pstring ")")
                    wrap(nP <|> sP)
        }

    | Shape.FSharpList s ->
        s.Accept {
            new IFSharpListVisitor<Parser<'T,unit>> with
                member __.Visit<'t> () =
                    let tp = mkFParser<'t>()
                    let sep = spaces >>. pstring ";" >>. spaces
                    let lp = pstring "[" >>. spaces >>. sepBy tp sep .>> spaces .>> pstring "]"
                    wrap lp
        }

    | Shape.Array s ->
        s.Accept {
            new IArrayVisitor<Parser<'T,unit>> with
                member __.Visit<'t> () =
                    let tp = mkFParser<'t> ()
                    let sep = spaces >>. pstring ";" >>. spaces
                    let lp = pstring "[|" >>. spaces >>. sepBy tp sep .>> spaces .>> pstring "|]"
                    wrap(lp |>> Array.ofList)
        }

    | Shape.Tuple2 s ->
        s.Accept {
            new ITuple2Visitor<Parser<'T,unit>> with
                member __.Visit<'t1, 't2> () =
                    let tp = spaces >>. mkFParser<'t1>() .>> spaces .>> pstring "," .>> spaces
                    let sp = spaces >>. mkFParser<'t2>() .>> spaces
                    wrap(pstring "(" >>. tuple2 tp sp .>> pstring ")")
        }

    | Shape.Tuple3 s ->
        s.Accept {
            new ITuple3Visitor<Parser<'T,unit>> with
                member __.Visit<'t1, 't2, 't3> () =
                    let t1p = spaces >>. mkFParser<'t1>() .>> spaces .>> pstring "," .>> spaces
                    let t2p = spaces >>. mkFParser<'t2>() .>> spaces .>> pstring "," .>> spaces
                    let t3p = spaces >>. mkFParser<'t3>() .>> spaces
                    wrap(pstring "(" >>. tuple3 t1p t2p t3p .>> pstring ")")
        }

    | Shape.FSharpRecord1 s ->
        s.Accept {
            new IFSharpRecord1Visitor<Parser<'T,unit>> with
                member __.Visit (s : IShapeFSharpRecord<'Record, 'Field1>) =
                    let f1p = spaces >>. pstring s.Properties.[0].Name >>. spaces >>. pstring "=" >>. spaces >>. mkFParser<'Field1>() .>> spaces
                    wrap(pstring "{" >>. f1p |>> s.Construct .>> pstring "}")
        }

    | Shape.FSharpRecord2 s ->
        s.Accept {
            new IFSharpRecord2Visitor<Parser<'T,unit>> with
                member __.Visit<'Record,'Field1,'Field2> (s : IShapeFSharpRecord<'Record,'Field1,'Field2>) =
                    let f1p = spaces >>. pstring s.Properties.[0].Name >>. spaces >>. pstring "=" >>. spaces >>. mkFParser<'Field1>() .>> spaces .>> pstring ";" .>> spaces
                    let f2p = spaces >>. pstring s.Properties.[1].Name >>. spaces >>. pstring "=" >>. spaces >>. mkFParser<'Field2>() .>> spaces
                    wrap(pstring "{" >>. tuple2 f1p f2p |>> s.Construct .>> pstring "}")
        }

    | Shape.FSharpUnion1 s ->
        s.Accept {
            new IFSharpUnion1Visitor<Parser<'T,unit>> with
                member __.Visit (s : IShapeFSharpUnion<'Union,'Case1>) =
                    let c1p = mkFParser<'Case1>() |>> s.Construct1
                    let u1p = pstring s.UnionCaseInfo.[0].Name >>. spaces >>. pstring "(" >>. c1p .>> pstring ")"
                    wrap c1p
        }

    | Shape.FSharpUnion2 s ->
        s.Accept {
            new IFSharpUnion2Visitor<Parser<'T,unit>> with
                member __.Visit<'Union,'Case1,'Case2> (s : IShapeFSharpUnion<'Union,'Case1,'Case2>) =
                    let c1p, c2p = mkFParser<'Case1>() |>> s.Construct1, mkFParser<'Case2>() |>> s.Construct2
                    let u1p = pstring s.UnionCaseInfo.[0].Name >>. spaces >>. pstring "(" >>. c1p .>> pstring ")"
                    let u2p = pstring s.UnionCaseInfo.[1].Name >>. spaces >>. pstring "(" >>. c2p .>> pstring ")"
                    wrap(u1p <|> u2p)
        }

    | _ -> failwithf "unsupported type '%O'" typeof<'T>


let p1 = mkParser<int * int list>()
p1 "(42, [1;2;3])"

let p2 = mkParser<int * string list option * string ref>()
p2 """(42, Some (["1" ;  "2"]), { contents= "value" }) ) """