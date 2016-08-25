#load "../src/TypeShape/TypeShape.fs"

open System
open TypeShape

let rec mkPrinter<'T> () : 'T -> string = aux<'T>()
and private aux<'T> () : 'T -> string =
    let wrap(p : 'a -> string) = unbox<'T -> string> p
    match TypeShape.Create<'T>() with
    | Shape.Unit -> wrap(fun () -> "()")
    | Shape.Bool -> wrap(sprintf "%b")
    | Shape.Byte -> wrap(fun (b:byte) -> sprintf "%duy" b)
    | Shape.Int32 -> wrap(sprintf "%d")
    | Shape.Int64 -> wrap(fun (b:int64) -> sprintf "%dL" b)
    | Shape.String -> wrap(sprintf "\"%s\"")
    | Shape.FSharpOption s ->
        s.Accept {
            new IFSharpOptionVisitor<'T -> string> with
                member __.Visit<'a> () =
                    let tp = mkPrinter<'a>()
                    wrap(function None -> "None" | Some t -> sprintf "Some (%s)" (tp t))
        }

    | Shape.Tuple2 s ->
        s.Accept {
            new ITuple2Visitor<'T -> string> with
                member __.Visit<'t1, 't2> () =
                    let tp = mkPrinter<'t1>()
                    let sp = mkPrinter<'t2>()
                    wrap(fun (t : 't1, s : 't2) -> sprintf "(%s, %s)" (tp t) (sp s))
        }

    | Shape.Tuple3 s ->
        s.Accept {
            new ITuple3Visitor<'T -> string> with
                member __.Visit<'t1, 't2, 't3> () =
                    let t1p = mkPrinter<'t1>()
                    let t2p = mkPrinter<'t2>()
                    let t3p = mkPrinter<'t3>()
                    wrap(fun (t1 : 't1, t2 : 't2, t3 : 't3) -> sprintf "(%s, %s, %s)" (t1p t1) (t2p t2) (t3p t3))
        }

    | Shape.FSharpList s ->
        s.Accept {
            new IFSharpListVisitor<'T -> string> with
                member __.Visit<'a> () =
                    let tp = mkPrinter<'a>()
                    wrap(fun ts -> ts |> List.map tp |> String.concat "; " |> sprintf "[%s]")
        }

    | Shape.Array s ->
        s.Accept {
            new IArrayVisitor<'T -> string> with
                member __.Visit<'a> () =
                    let tp = mkPrinter<'a> ()
                    wrap(fun ts -> ts |> Array.map tp |> String.concat "; " |> sprintf "[|%s|]")
        }

    | Shape.FSharpSet s ->
        s.Accept {
            new IFSharpSetVisitor<'T -> string> with
                member __.Visit<'a when 'a : comparison> () =
                    let tp = mkPrinter<'a>()
                    wrap(fun (s:Set<'a>) -> s |> Seq.map tp |> String.concat "; " |> sprintf "set [%s]")
        }

    | Shape.FSharpRecord1 s ->
        s.Accept {
            new IFSharpRecord1Visitor<'T -> string> with
                member __.Visit (s : IShapeFSharpRecord<'Record, 'Field1>) =
                    let f1p = mkPrinter<'Field1>()
                    let n1 = s.Properties.[0].Name
                    wrap(fun (r:'Record) -> sprintf "{ %s = %s }" n1 (s.Project1 r |> f1p))
        }

    | Shape.FSharpRecord2 s ->
        s.Accept {
            new IFSharpRecord2Visitor<'T -> string> with
                member __.Visit<'Record,'Field1,'Field2> (s : IShapeFSharpRecord<'Record,'Field1,'Field2>) =
                    let f1p,f2p = mkPrinter<'Field1>(), mkPrinter<'Field2>()
                    let n1,n2 = s.Properties.[0].Name, s.Properties.[1].Name
                    wrap(fun (r:'Record) -> sprintf "{ %s = %s ; %s = %s }" n1 (s.Project1 r |> f1p) n2 (s.Project2 r |> f2p))
        }

    | Shape.FSharpUnion1 s ->
        s.Accept {
            new IFSharpUnion1Visitor<'T -> string> with
                member __.Visit (s : IShapeFSharpUnion<'Union,'Case1>) =
                    let c1p = mkPrinter<'Case1>()
                    let n1 = s.UnionCaseInfo.[0].Name
                    wrap(fun (u:'Union) -> sprintf "%s %s" n1 (s.Project u |> c1p))
        }

    | Shape.FSharpUnion2 s ->
        s.Accept {
            new IFSharpUnion2Visitor<'T -> string> with
                member __.Visit<'Union,'Case1,'Case2> (s : IShapeFSharpUnion<'Union,'Case1,'Case2>) =
                    let c1p, c2p = mkPrinter<'Case1>(), mkPrinter<'Case2>()
                    let n1,n2 = s.UnionCaseInfo.[0].Name, s.UnionCaseInfo.[1].Name
                    wrap(fun (u:'Union) ->
                        match s.Project u with
                        | Choice1Of2 c1 -> sprintf "%s %s" n1 (c1p c1)
                        | Choice2Of2 c2 -> sprintf "%s %s" n2 (c2p c2))
        }

    | _ -> failwithf "unsupported type '%O'" typeof<'T>



type Foo = { A : int ; B : string }
type Bar = A of int * string | B of int

let p = mkPrinter<(int list * string option) * (bool * unit) * (Foo * Bar list)> ()
p (([1 .. 5], None), (false, ()), ({A = 42 ; B = "42"}, [A(2,"42") ; B 12]))

//
// Some benchmarks
//

#time "on"

type TestType = (int list * string option * string) * (bool * unit)
let value : TestType = (([1 .. 5], None, "42"), (false, ()))

let p1 = sprintf "%A" : TestType -> string
let p2 = mkPrinter<TestType>()

p1 value
p2 value

// Real: 00:00:00.442, CPU: 00:00:00.437, GC gen0: 31, gen1: 0, gen2: 0
for i = 1 to 1000 do ignore <| p1 value
// Real: 00:00:00.006, CPU: 00:00:00.000, GC gen0: 2, gen1: 1, gen2: 0
for i = 1 to 1000 do ignore <| p2 value