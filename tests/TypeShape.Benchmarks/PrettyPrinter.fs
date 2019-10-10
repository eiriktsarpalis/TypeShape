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

let baselinePrinter ((rss, sss, us) : TestType) =
    let sb = new System.Text.StringBuilder()
    let inline append (x:string) = sb.Append x |> ignore

    let printRecord (r : Record) = sprintf "{ A = \"%s\" ; B = %d ; C = %b }" r.A r.B r.C |> append
    let printUnion (u : Union) =
        match u with
        | A n -> sprintf "A %d" n |> append
        | B -> append "B"
        | C(x,y) -> sprintf "C(\"%s\", %d) x y" x y |> append

    append "("
    append "["
    let mutable first = true
    for rs in rss do
        if first then first <- false else append "; "
        append "["
        let mutable first = true
        for r in rs do
            if first then first <- false else append "; "
            printRecord r
        append "]"
    append "]"

    append ", "
    append "[|"
    let mutable first = true
    for ss in sss do
        if first then first <- false else append "; "
        append "["
        let mutable first = true
        for s in ss do
            if first then first <- false else append "; "
            append "\""
            append s
            append "\""
        append "]"

    append "|]"

    append ", "
    append "[|"
    let mutable first = true
    for u in us do
        if first then first <- false else append "; "
        printUnion u
    append "|]"
    append ")"

    sb.ToString()

let fsharpCorePrinter (value : TestType) =
    sprintf "%A" value

let typeShapePrinter = Printer.mkPrinter<TestType>()

let testValue : TestType = 
    struct(
        [ [{ A = "value" ; B = 42 ; C = false }]; []; [{A = "A'" ; B = 0 ; C = true}] ],
        [| [] ; ["A";"B"] |], 
        [|A 42; B; B ; C("value", 0)|])

type PrettyPrinterBenchmarks() =
    [<Benchmark(Description = "Baseline PrettyPrinter", Baseline = true)>]
    member __.Baseline() = baselinePrinter testValue |> ignore
    [<Benchmark(Description = "FSharp.Core PrettyPrinter")>]
    member __.FSharpCore() = fsharpCorePrinter testValue |> ignore
    [<Benchmark(Description = "TypeShape PrettyPrinter")>]
    member __.TypeShape() = typeShapePrinter testValue |> ignore