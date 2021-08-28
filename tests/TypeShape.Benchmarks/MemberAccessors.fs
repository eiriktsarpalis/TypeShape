module TypeShape.Benchmarks.MemberAccessors

open System
open BenchmarkDotNet.Attributes
open TypeShape.Core

let inline defautl<'T> = Unchecked.defaultof<'T>

// Type under test
type Record = { mutable A : string ; mutable B : int ; mutable C : bool }

type Union =
    | A of int
    | B of string * bool
    | C

type TupleType = (struct(int * int * int * int * int * int * int * int * int * int))

// Handwritten member accessors
type Lens<'DeclaringType,'Field> = { get : Func<'DeclaringType, 'Field> ; set : Func<'DeclaringType, 'Field, 'DeclaringType> }

let defaultConstructor : Func<Record> = Func<_>(fun () -> { A = defautl ; B = defautl ; C = defautl })
let stringLens = { get = Func<Record,_>(fun r -> r.A) ; set = Func<_,_,_>(fun r v -> r.A <- v; r) }
let intLens = { get = Func<Record,_>(fun r -> r.B) ; set = Func<_,_,_>(fun r v -> r.B <- v; r) }
let boolLens = { get = Func<Record,_>(fun r -> r.C) ; set = Func<_,_,_>(fun r v -> r.C <- v; r) }
let tupleLens = { get = Func<TupleType,_>(function (struct(_,_,_,_,_,_,_,_,_,x)) -> x) ; set = Func<_,_,_>(fun (struct(x1,x2,x3,x4,x5,x6,x7,x8,x9,_)) y -> (x1,x2,x3,x4,x5,x6,x7,x8,x9,y)) }
let unionTagReader = Func<_,_>(function A _ -> 0 | B _ -> 1 | C -> 2)

// TypeShape member accessors
let recordShape =
    match shapeof<Record> with
    | Shape.FSharpRecord (:? ShapeFSharpRecord<Record> as shape) -> shape
    | _ -> failwith "should not fail"

let stringMemberShape = recordShape.Fields.[0] :?> ShapeMember<Record, string>
let intMemberShape = recordShape.Fields.[1] :?> ShapeMember<Record, int>
let boolMemberShape = recordShape.Fields.[2] :?> ShapeMember<Record, bool>

let unionShape =
    match shapeof<Union> with
    | Shape.FSharpUnion (:? ShapeFSharpUnion<Union> as shape) -> shape
    | _ -> failwith "should not fail"

let tupleShape =
    match shapeof<TupleType> with
    | Shape.Tuple (:? ShapeTuple<TupleType> as shape) -> shape
    | _ -> failwith "should not fail"

let tupleElemShape = tupleShape.Elements.[0] :?> ShapeMember<TupleType, int>

[<MemoryDiagnoser; DisassemblyDiagnoser(maxDepth = 3)>]
type MemberAccessorBenchmark() =

    let value = { A = "string" ; B = 42; C = false }
    let union = B("str", false)
    let tuple : TupleType = struct(1,1,1,1,1,1,1,1,1,1)

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

    [<Benchmark>]
    member _.GetTuple_BaseLine() = let struct(_,_,_,_,_,_,_,_,_,x) = tuple in x
    [<Benchmark>]
    member _.GetTuple_CustomLens() = tupleLens.get.Invoke(tuple)
    [<Benchmark>]
    member _.GetTuple_TypeShape() = tupleElemShape.Get tuple

    [<Benchmark>]
    member _.SetTuple_BaseLine() = let struct(x1,x2,x3,x4,x5,x6,x7,x8,x9,_) = tuple in (x1,x2,x3,x4,x5,x6,x7,x8,x9,42)
    [<Benchmark>]
    member _.SetTuple_CustomLens() = tupleLens.set.Invoke(tuple, 42)
    [<Benchmark>]
    member _.SetTuple_TypeShape() = tupleElemShape.Set tuple 42

    [<Benchmark>]
    member _.GetUnionTag_BaseLine() = match union with A _ -> 0 | B _ -> 1 | C -> 2
    [<Benchmark>]
    member _.GetUnionTag_CustomLens() = unionTagReader.Invoke(union)
    [<Benchmark>]
    member _.GetUnionTag_TypeShape() = unionShape.GetTag union