module TypeShape.Benchmarks.PrettyPrinter

open System
open BenchmarkDotNet.Attributes

type Record =
    { A : string ; B : int ; C : bool }

type Union =
    | A of int
    | B
    | C of string * int

type TestType = (struct(Record list list * string list [] * Union []))

let fsharpCorePrinter (value : TestType) =
    sprintf "%A" value

let typeShapePrinter = Printer.mkPrinter<TestType>()

let testValue : TestType = 
    struct(
        [ [{ A = "A" ; B = 42 ; C = false }]; []; [{A = "A'" ; B = 0 ; C = true}] ], 
        [| [] ; ["A";"B"] |], 
        [|A 42; B; B ; C("C", 0)|])

type PrettyPrinterBenchmarks() =
    [<Benchmark(Description = "FSharp.Core PrettyPrinter", Baseline = true)>]
    member __.FSharpCore() = fsharpCorePrinter testValue |> ignore
    [<Benchmark(Description = "TypeShape PrettyPrinter")>]
    member __.TypeShape() = typeShapePrinter testValue |> ignore