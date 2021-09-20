module TypeShape.Benchmarks.JsonSerializer

open System
open BenchmarkDotNet.Attributes

type Enum =
    | A = 0
    | B = 1
    | C = 3

[<CLIMutable>]
type Record = 
    { 
        FieldA : string ; 
        FieldB : int ; 
        FieldC : bool ;
        FieldD : DateTimeOffset;
        FieldE : Nullable<Enum> []
        FieldZ : string []
    }

type TestType = Record [][]

[<MemoryDiagnoser>]
type JsonSerializerBenchmarks() =
    let converter = JsonSerializer.generateConverter<TestType>()
    // Value to be tested
    let testValue : TestType = 
        [|  
            [| { FieldA = "A" ; FieldB = 42 ; FieldC = true ; FieldD = DateTimeOffset.MaxValue ; FieldE = [|Nullable Enum.A; Nullable Enum.C; Nullable Enum.B |] ; FieldZ = [||] } |]
            [| |]
            [| { FieldA = "B" ; FieldB = 0 ; FieldC = false ; FieldD = DateTimeOffset.MinValue ; FieldE = [|Nullable() ; Nullable Enum.B |] ; FieldZ = [||] } ;
               { FieldA = "C" ; FieldB = -1 ; FieldC = false ; FieldD = DateTimeOffset.MinValue ; FieldE = [||] ; FieldZ = [|"A";"B";"C"|] } |]
        |]
    let json = testValue |> System.Text.Json.JsonSerializer.Serialize

    [<Benchmark>]
    member _.Serialize_SystemTextJson() = testValue |> System.Text.Json.JsonSerializer.Serialize
        
    [<Benchmark>]
    member _.Serialize_TypeShape() = testValue |> JsonSerializer.serialize converter

    [<Benchmark>]
    member _.Deserialize_SystemTextJson() = json |> System.Text.Json.JsonSerializer.Deserialize<TestType>
        
    [<Benchmark>]
    member _.Deserialize_TypeShape() = json |> JsonSerializer.deserialize converter