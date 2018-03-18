module TypeShape.Benchmarks.Main

open BenchmarkDotNet.Running

[<EntryPoint>]
let main _ =
    
    let _summary = BenchmarkRunner.Run<UnionEncoder.Benchmark>()

    0
