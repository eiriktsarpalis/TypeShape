module TypeShape.Benchmarks.MemberAccessors

open System
open System.Reflection
open FSharp.Reflection
open BenchmarkDotNet.Attributes
open TypeShape.Core

let inline defautl<'T> = Unchecked.defaultof<'T>

// Type under test
type Record = { mutable A : string ; mutable B : int ; mutable C : bool }

// Handwritten member accessors
let defaultConstructor : Func<Record> = Func<_>(fun () -> { A = defautl ; B = defautl ; C = defautl })
type Lens<'Field> = { get : Func<Record, 'Field> ; set : Func<Record, 'Field, Record> }
let stringLens = { get = Func<_,_>(fun r -> r.A) ; set = Func<_,_,_>(fun r v -> r.A <- v; r) }
let intLens = { get = Func<_,_>(fun r -> r.B) ; set = Func<_,_,_>(fun r v -> r.B <- v; r) }
let boolLens = { get = Func<_,_>(fun r -> r.C) ; set = Func<_,_,_>(fun r v -> r.C <- v; r) }

// TypeShape member accessors
let recordShape =
    match shapeof<Record> with
    | Shape.FSharpRecord (:? ShapeFSharpRecord<Record> as shape) -> shape
    | _ -> failwith "should not fail"

let stringMemberShape = recordShape.Fields.[0] :?> ShapeMember<Record, string>
let intMemberShape = recordShape.Fields.[1] :?> ShapeMember<Record, int>
let boolMemberShape = recordShape.Fields.[2] :?> ShapeMember<Record, bool>

[<MemoryDiagnoser>]
type MemberAccessorBenchmark() =

    let value = { A = "string" ; B = 42; C = false }

    [<Benchmark>]
    member _.CreateRecord_BaseLine() = { A = defautl ; B = defautl ; C = defautl }
    [<Benchmark>]
    member _.CreateRecord_CustomLens() = defaultConstructor.Invoke()
    [<Benchmark>]
    member _.CreateRecord_TypeShape() = recordShape.CreateUninitialized()

    [<Benchmark>]
    member _.GetString_BaseLine() = value.A
    [<Benchmark>]
    member _.GetString_CustomLens() = stringLens.get.Invoke value
    [<Benchmark>]
    member _.GetString_TypeShape() = stringMemberShape.Get value

    [<Benchmark>]
    member _.SetString_BaseLine() = value.A <- "string"
    [<Benchmark>]
    member _.SetString_CustomLens() = stringLens.set.Invoke(value, "string")
    [<Benchmark>]
    member _.SetString_TypeShape() = stringMemberShape.Set value "string"

    [<Benchmark>]
    member _.GetInt_BaseLine() = value.B
    [<Benchmark>]
    member _.GetInt_CustomLens() = intLens.get.Invoke value
    [<Benchmark>]
    member _.GetInt_TypeShape() = intMemberShape.Get value

    [<Benchmark>]
    member _.SetInt_BaseLine() = value.B <- 42
    [<Benchmark>]
    member _.SetInt_CustomLens() = intLens.set.Invoke(value, 42)
    [<Benchmark>]
    member _.SetInt_TypeShape() = intMemberShape.Set value 42