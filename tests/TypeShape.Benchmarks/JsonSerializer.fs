module TypeShape.Benchmarks.JsonSerializer

open System
open Newtonsoft.Json
open MBrace.FsPickler.Json
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

// Value to be tested
let testValue : TestType = 
    [|  
        [| { FieldA = "A" ; FieldB = 42 ; FieldC = true ; FieldD = DateTimeOffset.MaxValue ; FieldE = [|Nullable Enum.A; Nullable Enum.C; Nullable Enum.B |] ; FieldZ = [||] } |]
        [| |]
        [| { FieldA = "B" ; FieldB = 0 ; FieldC = false ; FieldD = DateTimeOffset.MinValue ; FieldE = [|Nullable() ; Nullable Enum.B |] ; FieldZ = [||] } ;
           { FieldA = "C" ; FieldB = -1 ; FieldC = false ; FieldD = DateTimeOffset.MinValue ; FieldE = [||] ; FieldZ = [|"A";"B";"C"|] } |]
    |]

[<MemoryDiagnoser>]
type JsonSerializerBenchmarks() =
    let converter = JsonSerializer.generateConverter<TestType>()
    let fspickler = FsPickler.CreateJsonSerializer()
    let stjJson = testValue |> System.Text.Json.JsonSerializer.Serialize
    let newtonsoftJson = testValue |> JsonConvert.SerializeObject
    let fspicklerJson = testValue |> fspickler.PickleToString
    let tsJson = testValue |> JsonSerializer.serialize converter

    [<Benchmark>]
    member _.Serialize_SystemTextJson() = testValue |> System.Text.Json.JsonSerializer.Serialize

    [<Benchmark>]
    member _.Serialize_Newtonsoft() = testValue |> JsonConvert.SerializeObject

    [<Benchmark>]
    member _.Serialize_FsPickler() = testValue |> fspickler.PickleToString
        
    [<Benchmark>]
    member _.Serialize_TypeShape() = testValue |> JsonSerializer.serialize converter

    [<Benchmark>]
    member _.Deserialize_SystemTExtJson() = stjJson |> System.Text.Json.JsonSerializer.Deserialize<TestType>

    [<Benchmark>]
    member _.Deserialize_Newtonsoft() = newtonsoftJson |> JsonConvert.DeserializeObject<TestType>

    [<Benchmark>]
    member _.Deserialize_FsPickler() = fspicklerJson |> fspickler.UnPickleOfString<TestType>
        
    [<Benchmark>]
    member _.Deserialize_TypeShape() = tsJson |> JsonSerializer.deserialize converter