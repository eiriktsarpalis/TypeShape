module TypeShape.Benchmarks.Main

open BenchmarkDotNet.Running

[<EntryPoint>]
let main args =

    printfn "%s" (typeof<FSharp.Quotations.Evaluator.QuotationEvaluator>.Assembly.Location)

    let assembly = System.Reflection.Assembly.GetExecutingAssembly()
    let switcher = new BenchmarkSwitcher(assembly)
    let summaries = switcher.Run(args)


    0
