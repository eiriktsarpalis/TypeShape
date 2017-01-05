#r "../bin/TypeShape.dll"

open System
open TypeShape
open TypeShape_Utils

// Generic value printer with recursive type support

let rec mkPrinter<'T> () : 'T -> string =
    let state = new RecTypeManager()
    mkPrinterCached<'T> state

and mkPrinterCached<'T> (state : RecTypeManager) : 'T -> string =
    match state.TryGetValue<'T -> string>() with
    | Some p -> p
    | None ->
        let _ = state.Create<'T -> string>(fun f t -> f () t)
        let r = mkPrinterAux<'T> state
        state.Complete r
        r

and mkPrinterAux<'T> (state : RecTypeManager) : 'T -> string =
    let wrap(p : 'a -> string) = unbox<'T -> string> p
    let mkFieldPrinter (field : IShapeMember<'DeclaringType>) =
        field.Accept {
            new IMemberVisitor<'DeclaringType, string * ('DeclaringType -> string)> with
                member __.Visit(field : ShapeMember<'DeclaringType, 'Field>) =
                    let fp = mkPrinterCached<'Field> state
                    field.Label, fun (r:'DeclaringType) -> field.Project r |> fp
        }

    match TypeShape.Create<'T>() with
    | Shape.Unit -> wrap(fun () -> "()") // 'T = unit
    | Shape.Bool -> wrap(sprintf "%b") // 'T = bool
    | Shape.Byte -> wrap(fun (b:byte) -> sprintf "%duy" b) // 'T = byte
    | Shape.Int32 -> wrap(sprintf "%d")
    | Shape.Int64 -> wrap(fun (b:int64) -> sprintf "%dL" b)
    | Shape.String -> wrap(sprintf "\"%s\"")
    | Shape.FSharpOption s ->
        s.Accept {
            new IFSharpOptionVisitor<'T -> string> with
                member __.Visit<'a> () = // 'T = 'a option
                    let tp = mkPrinterCached<'a> state
                    wrap(function None -> "None" | Some t -> sprintf "Some (%s)" (tp t))
        }

    | Shape.FSharpList s ->
        s.Accept {
            new IFSharpListVisitor<'T -> string> with
                member __.Visit<'a> () = // 'T = 'a list
                    let tp = mkPrinterCached<'a> state
                    wrap(fun (ts : 'a list) -> ts |> Seq.map tp |> String.concat "; " |> sprintf "[%s]")
        }

    | Shape.Array s when s.Rank = 1 ->
        s.Accept {
            new IArrayVisitor<'T -> string> with
                member __.Visit<'a> _ = // 'T = 'a []
                    let tp = mkPrinterCached<'a> state
                    wrap(fun (ts : 'a []) -> ts |> Seq.map tp |> String.concat "; " |> sprintf "[|%s|]")
        }

    | Shape.FSharpSet s ->
        s.Accept {
            new IFSharpSetVisitor<'T -> string> with
                member __.Visit<'a when 'a : comparison> () =  // 'T = Set<'a>
                    let tp = mkPrinterCached<'a> state
                    wrap(fun (s : Set<'a>) -> s |> Seq.map tp |> String.concat "; " |> sprintf "set [%s]")
        }

    | Shape.Tuple s ->
        s.Accept {
            new ITupleVisitor<'T -> string> with
                member __.Visit (s : ShapeTuple<'Tuple>) = // 'Tuple = 'T
                    let elemPrinters = s.Elements |> Array.map mkFieldPrinter
                    wrap(fun (t:'Tuple) ->
                        elemPrinters
                        |> Seq.map (fun (_,ep) -> let value = ep t in value)
                        |> String.concat ", "
                        |> sprintf "(%s)")
        }

    | Shape.FSharpRecord s ->
        s.Accept {
            new IFSharpRecordVisitor<'T -> string> with
                member __.Visit (s : ShapeFSharpRecord<'Record>) = // 'Record = 'T
                    let fieldPrinters = s.Fields |> Array.map mkFieldPrinter
                    wrap(fun (r:'Record) ->
                        fieldPrinters
                        |> Seq.map (fun (label, ep) -> let value = ep r in sprintf "%s = %s" label value)
                        |> String.concat "; "
                        |> sprintf "{ %s }")
        }

    | Shape.FSharpUnion s ->
        s.Accept {
            new IFSharpUnionVisitor<'T -> string> with
                member __.Visit (s : ShapeFSharpUnion<'Union>) = // 'Union = 'T
                    let mkUnionCasePrinter (s : ShapeFSharpUnionCase<'Union>) =
                        let fieldPrinters = s.Fields |> Array.map mkFieldPrinter
                        fun (u:'Union) -> 
                            match fieldPrinters with
                            | [||] -> s.CaseInfo.Name
                            | [|_,fp|] -> sprintf "%s %s" s.CaseInfo.Name (fp u)
                            | fps ->
                                fps
                                |> Seq.map (fun (_,fp) -> fp u)
                                |> String.concat ", "
                                |> sprintf "%s(%s)" s.CaseInfo.Name

                    let casePrinters = s.UnionCases |> Array.map mkUnionCasePrinter
                    wrap(fun (u:'Union) -> 
                        let printer = casePrinters.[s.GetTag u]
                        printer u)
        }

    | _ -> failwithf "unsupported type '%O'" typeof<'T>



type Foo = { A : int ; B : string ; C : int list }
type Bar = A of int * string | B of int

let p = mkPrinter<int list * string option * bool * unit * Foo * Bar list> ()
p ([1 .. 5], None, false, (), {A = 42 ; B = "42" ; C = [42] }, [A(2,"42") ; B 12])

//
// Some benchmarks
//

#time "on"

type TestType = (int list * string option * string) * (bool * unit) * Bar
let value : TestType = (([1 .. 5], None, "42"), (false, ()), B 12)

let p1 = sprintf "%A" : TestType -> string
let p2 = mkPrinter<TestType>()

p1 value
p2 value

// Real: 00:00:00.924, CPU: 00:00:00.921, GC gen0: 41, gen1: 1, gen2: 1
for i = 1 to 1000 do ignore <| p1 value
// Real: 00:00:00.017, CPU: 00:00:00.015, GC gen0: 3, gen1: 0, gen2: 0
for i = 1 to 1000 do ignore <| p2 value


//
//  Recursive types
//

type Peano = Z | S of Peano

let rec int2Peano = function 0 -> Z | n -> S(int2Peano(n-1))

let printer = mkPrinter<Peano>()

int2Peano 10 |> printer