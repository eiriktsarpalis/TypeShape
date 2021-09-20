module TypeShape.Benchmarks.Main

open BenchmarkDotNet.Running
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Jobs

// by default BDN will hardcode 'Release'
let configuration = 
#if DEBUG
    "Debug"
#else
    "Release"
#endif
#if !TYPESHAPE_EMIT
    + "-NoEmit"
#endif

[<EntryPoint>]
let main args =
    let assembly = System.Reflection.Assembly.GetExecutingAssembly()
    let switcher = new BenchmarkSwitcher(assembly)
    let summaries = switcher.Run(args, DefaultConfig.Instance.AddJob(Job.Default.WithCustomBuildConfiguration(configuration)))
    0