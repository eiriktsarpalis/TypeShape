#r "../../bin/TypeShape.dll"
open System
open TypeShape

let rec mkPrinter<'T> () : 'T -> string = mkPrinterUntyped typeof<'T> :?> _
and private mkPrinterUntyped (t : Type) : obj =
    match TypeShape.Resolve t with
    | Shape.Unit -> box(fun () -> "()")
    | Shape.Bool -> box(sprintf "%b")
    | Shape.Int32 -> box(sprintf "%d")
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

    | Shape.Tuple2 s ->
        s.Accept {
            new ITuple2Visitor<obj> with
                member __.Visit<'T, 'S> () =
                    let tp = mkPrinter<'T>()
                    let sp = mkPrinter<'S>()
                    box(fun (t : 'T, s : 'S) -> sprintf "(%s, %s)" (tp t) (sp s))
        }

    | Shape.FSharpRecord2 s ->
        s.Accept {
            new IFSharpRecord2Visitor<obj> with
                member __.Visit (s : IShapeFSharpRecord<'Record,'Field1,'Field2>) =
                    let f1p,f2p = mkPrinter<'Field1>(), mkPrinter<'Field2>()
                    let n1,n2 = s.Properties.[0].Name, s.Properties.[1].Name
                    box(fun (r:'Record) -> sprintf "{ %s = %s ; %s = %s }" n1 (s.Project1 r |> f1p) n2 (s.Project2 r |> f2p))
        }

    | Shape.FSharpUnion2 s ->
        s.Accept {
            new IFSharpUnion2Visitor<obj> with
                member __.Visit (s : IShapeFSharpUnion<'Union,'Case1,'Case2>) =
                    let c1p, c2p = mkPrinter<'Case1>(), mkPrinter<'Case2>()
                    let n1,n2 = s.UnionCaseInfo.[0].Name, s.UnionCaseInfo.[1].Name
                    box(fun (u:'Union) ->
                        match s.Project u with
                        | Choice1Of2 c1 -> sprintf "%s %s" n1 (c1p c1)
                        | Choice2Of2 c2 -> sprintf "%s %s" n2 (c2p c2))
        }

    | _ -> failwithf "unsupported type '%O'" t



let p = mkPrinter<(int list * string option) * (bool * unit)> ()
p (([1 .. 5], None), (false, ()))

type Foo = { A : int ; B : string }
type Bar = A of int * string | B of int

let q = mkPrinter<Foo * Bar list>()

q ({ A =2 ; B = "42"}, [B 42; A(2,"42")])

#time "on"
let value = (([1 .. 5], Some "42"), (false, ()))

// Real: 00:00:00.561, CPU: 00:00:00.562, GC gen0: 32, gen1: 0, gen2: 0
for i = 1 to 1000 do ignore <| sprintf "%A" value
// Real: 00:00:00.010, CPU: 00:00:00.000, GC gen0: 1, gen1: 0, gen2: 0
for i = 1 to 1000 do ignore <| p value