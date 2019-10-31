module TypeShape.Benchmarks.Empty

open System
open System.Reflection
open FSharp.Reflection
open TypeShape.Empty
open BenchmarkDotNet.Attributes

type Union =
    | A of int * string
    | B of string
    | C

type Record =
    { A : string ; B : Union ; C : bool }

type TestType = (Record * Record) option * string [] option * int []

// baseline empty implementation
let baselineEmpty () =
    (Some ({ A = "" ; B = C ; C = false }, { A = "" ; B = C ; C = false }), Some [|""|], [|0|])

// datatype generic reflection-based implementation
let rec mkReflectionEmpty (t : Type) : unit -> obj =
    if t = typeof<string> then (fun () -> box "")
    elif t = typeof<int> then (fun () -> box 0)
    elif t = typeof<bool> then (fun () -> box false)
    elif t.IsArray then
        let et = t.GetElementType()
        let elem = mkReflectionEmpty et
        (fun () -> let arr = Array.CreateInstance(et, 1) in arr.SetValue(elem(), 0) ; box arr)
    elif FSharpType.IsTuple t then
        let elems = 
            FSharpType.GetTupleElements t
            |> Array.map mkReflectionEmpty

        let ctor = FSharpValue.PreComputeTupleConstructor(t)
        (fun () -> elems |> Array.map (fun e -> e()) |> ctor)
    elif FSharpType.IsRecord t then
        let elems =
            FSharpType.GetRecordFields t
            |> Array.map (fun p -> mkReflectionEmpty p.PropertyType)

        let ctor = FSharpValue.PreComputeRecordConstructor(t, true)
        (fun () -> elems |> Array.map (fun e -> e()) |> ctor)
    elif FSharpType.IsUnion t then
        let case = FSharpType.GetUnionCases(t, true) |> Array.maxBy(fun c -> c.GetFields().Length)
        let fields = case.GetFields() |> Array.map (fun p -> mkReflectionEmpty p.PropertyType)
        let ctor = FSharpValue.PreComputeUnionConstructor(case, true)
        (fun () -> fields |> Array.map (fun e -> e()) |> ctor)
    else
        failwithf "unrecognized type %O" t

let reflectionEmpty = mkReflectionEmpty typeof<TestType>

// typeshape implementation    
let typeShapeEmpty () = notEmpty<TestType>

[<MemoryDiagnoser>]
type EmptyBenchmark() =

    [<Benchmark(Description = "Baseline Empty", Baseline = true)>]
    member __.Baseline() = baselineEmpty() |> ignore
    [<Benchmark(Description = "Reflection Empty")>]
    member __.Reflection() = baselineEmpty() |> ignore
    [<Benchmark(Description = "TypeShape Empty")>]
    member __.TypeShape() = typeShapeEmpty() |> ignore
