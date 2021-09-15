module TypeShape.Benchmarks.Clone

open BenchmarkDotNet.Attributes

open TypeShape.Clone
open TypeShape.Tests.StagedClone

type Record =
    { A : string ; B : int ; C : bool }

type TestType = (struct(Record list list * string list [] * int option []))

[<MemoryDiagnoser>]
type CloneBenchmarks() =

    let baselineCloner : TestType -> TestType =
        fun (struct(v1,v2,v3)) ->

            let cloneRecord (r : Record) =
                { A = r.A ; B = r.B ; C = r.C }

            let v1' = v1 |> List.map (List.map cloneRecord)
            let v2' = v2 |> Array.map (List.map id)
            let v3' = v3 |> Array.map id
            struct(v1',v2',v3')

    let typeShapeCloner : TestType -> TestType =
        clone<TestType>

    let compiledStagedCloner : TestType -> TestType =
        mkCompiledCloner<TestType>()

    let testValue : TestType =
        let rs = [ for i in 1 .. 100 -> { A = sprintf "lorem ipsum %d" i ; B = i ; C = i % 2 = 0 } ]
        let ss = [for i in 1 .. 20 -> string i]
        struct([rs; []], [|ss|], [|for i in 1 .. 20 -> Some i|])

    [<Benchmark(Description = "Baseline Cloner", Baseline = true)>]
    member _.Baseline() = baselineCloner testValue |> ignore
    [<Benchmark(Description = "TypeShape Cloner")>]
    member _.TypeShape() = typeShapeCloner testValue |> ignore
    [<Benchmark(Description = "TypeShape Staged Cloner with compilation")>]
    member _.TypeShape_Staged() = compiledStagedCloner testValue |> ignore