#load "../src/TypeShape/TypeShape.fs"

open System
open TypeShape

let rec mkPrinter<'T> () : 'T -> string = mkPrinterUntyped typeof<'T> :?> _
and private mkPrinterUntyped (t : Type) : obj =
    match TypeShape.Create t with
    | Shape.Unit -> box(fun () -> "()")
    | Shape.Bool -> box(sprintf "%b")
    | Shape.Byte -> box(fun (b:byte) -> sprintf "%duy" b)
    | Shape.Int32 -> box(sprintf "%d")
    | Shape.Int64 -> box(fun (b:int64) -> sprintf "%dL" b)
    | Shape.String -> box(sprintf "\"%s\"")
    | Shape.FSharpOption s ->
        s.Accept {
            new IFSharpOptionVisitor<obj> with
                member __.Visit<'T> () =
                    let tp = mkPrinter<'T>()
                    box(function None -> "None" | Some t -> sprintf "Some (%s)" (tp t))
        }

    | Shape.FSharpList s ->
        s.Accept {
            new IFSharpListVisitor<obj> with
                member __.Visit<'T> () =
                    let tp = mkPrinter<'T>()
                    box(fun ts -> ts |> List.map tp |> String.concat "; " |> sprintf "[%s]")
        }

    | Shape.Array s ->
        s.Accept {
            new IArrayVisitor<obj> with
                member __.Visit<'T> () =
                    let tp = mkPrinter<'T> ()
                    box(fun ts -> ts |> Array.map tp |> String.concat "; " |> sprintf "[|%s|]")
        }

    | Shape.Tuple2 s ->
        s.Accept {
            new ITuple2Visitor<obj> with
                member __.Visit<'T, 'S> () =
                    let tp = mkPrinter<'T>()
                    let sp = mkPrinter<'S>()
                    box(fun (t : 'T, s : 'S) -> sprintf "(%s, %s)" (tp t) (sp s))
        }

    | Shape.Tuple3 s ->
        s.Accept {
            new ITuple3Visitor<obj> with
                member __.Visit<'T1, 'T2, 'T3> () =
                    let t1p = mkPrinter<'T1>()
                    let t2p = mkPrinter<'T2>()
                    let t3p = mkPrinter<'T3>()
                    box(fun (t1 : 'T1, t2 : 'T2, t3 : 'T3) -> sprintf "(%s, %s, %s)" (t1p t1) (t2p t2) (t3p t3))
        }

    | Shape.FSharpRecord1 s ->
        s.Accept {
            new IFSharpRecord1Visitor<obj> with
                member __.Visit (s : IShapeFSharpRecord<'Record, 'Field1>) =
                    let f1p = mkPrinter<'Field1>()
                    let n1 = s.Properties.[0].Name
                    box(fun (r:'Record) -> sprintf "{ %s = %s }" n1 (s.Project1 r |> f1p))
        }

    | Shape.FSharpRecord2 s ->
        s.Accept {
            new IFSharpRecord2Visitor<obj> with
                member __.Visit<'Record,'Field1,'Field2> (s : IShapeFSharpRecord<'Record,'Field1,'Field2>) =
                    let f1p,f2p = mkPrinter<'Field1>(), mkPrinter<'Field2>()
                    let n1,n2 = s.Properties.[0].Name, s.Properties.[1].Name
                    box(fun (r:'Record) -> sprintf "{ %s = %s ; %s = %s }" n1 (s.Project1 r |> f1p) n2 (s.Project2 r |> f2p))
        }

    | Shape.FSharpUnion1 s ->
        s.Accept {
            new IFSharpUnion1Visitor<obj> with
                member __.Visit (s : IShapeFSharpUnion<'Union,'Case1>) =
                    let c1p = mkPrinter<'Case1>()
                    let n1 = s.UnionCaseInfo.[0].Name
                    box(fun (u:'Union) -> sprintf "%s %s" n1 (s.Project u |> c1p))
        }

    | Shape.FSharpUnion2 s ->
        s.Accept {
            new IFSharpUnion2Visitor<obj> with
                member __.Visit<'Union,'Case1,'Case2> (s : IShapeFSharpUnion<'Union,'Case1,'Case2>) =
                    let c1p, c2p = mkPrinter<'Case1>(), mkPrinter<'Case2>()
                    let n1,n2 = s.UnionCaseInfo.[0].Name, s.UnionCaseInfo.[1].Name
                    box(fun (u:'Union) ->
                        match s.Project u with
                        | Choice1Of2 c1 -> sprintf "%s %s" n1 (c1p c1)
                        | Choice2Of2 c2 -> sprintf "%s %s" n2 (c2p c2))
        }

    | _ -> failwithf "unsupported type '%O'" t



type Foo = { A : int ; B : string }
type Bar = A of int * string | B of int

let p = mkPrinter<(int list * string option) * (bool * unit) * (Foo * Bar list)> ()
p (([1 .. 5], None), (false, ()), ({A = 42 ; B = "42"}, [A(2,"42") ; B 12]))