module TypeShape.Benchmarks.Main

open BenchmarkDotNet.Running

//type Benchmark = UnionEncoder.Benchmark
type Benchmark = Empty.Benchmark

[<EntryPoint>]
let main _ =
    
    let _summary = BenchmarkRunner.Run<Benchmark>()

    0
