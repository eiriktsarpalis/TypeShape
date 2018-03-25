module TypeShape.Benchmarks.Empty

open TypeShape.Empty
open BenchmarkDotNet.Attributes

type Union =
    | A of int * string
    | B of string
    | C

type Record =
    { A : string ; B : Union ; C : bool }

type TestType = (Record * Record) option * string list option * int []

let baselineEmpty () =
    (Some ({ A = "" ; B = C ; C = false }, { A = "" ; B = C ; C = false }), Some [""], [|0|])
    
let typeShapeEmpty () = notEmpty<TestType>

type EmptyBenchmark() =

    [<Benchmark(Description = "Baseline Empty", Baseline = true)>]
    member __.Baseline() = baselineEmpty() |> ignore
    [<Benchmark(Description = "TypeShape Empty")>]
    member __.TypeShape() = typeShapeEmpty() |> ignore