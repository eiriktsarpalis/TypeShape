module TypeShape.Benchmarks.MemberAccessors

open System
open System.Runtime.CompilerServices
open BenchmarkDotNet.Attributes
open TypeShape.Core

let inline defautl<'T> = Unchecked.defaultof<'T>

// Type under test
type Record = { mutable A : string ; mutable B : int ; mutable C : bool }

type Union =
    | A of int
    | B of string * bool
    | C
with
    member inline u.GetTag() =
        match u with
        | A _ -> 0
        | B _ -> 1
        | C _ -> 2

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    member u.GetTag_NoInlining() =
        match u with
        | A _ -> 0
        | B _ -> 1
        | C _ -> 2

type TupleType = int * int * int * int * int * int * int * int * int * int
type StructTupleType = (struct(int * int * int * int * int * int * int * int * int * int))

type Getter<'DeclaringType, 'Field> = delegate of inref<'DeclaringType> -> 'Field
type Setter<'DeclaringType, 'Field> = delegate of byref<'DeclaringType> * 'Field -> unit
type Lens<'DeclaringType,'Field> = { get : Getter<'DeclaringType, 'Field> ; set : Setter<'DeclaringType, 'Field> }

[<MemoryDiagnoser; DisassemblyDiagnoser(maxDepth = 3)>]
type MemberAccessorBenchmark() =

    // Handwritten member accessors
    let defaultTupleConstructor : Func<TupleType> = Func<_>(fun () -> (0,0,0,0,0,0,0,0,0,0))
    let defaultStructTupleConstructor : Func<StructTupleType> = Func<_>(fun () -> struct(0,0,0,0,0,0,0,0,0,0))
    let defaultConstructor : Func<Record> = Func<_>(fun () -> { A = defautl ; B = defautl ; C = defautl })

    let stringLens = { get = Getter<Record,_>(fun r -> r.A) ; set = Setter<_,_>(fun r v -> r.A <- v) }
    let intLens = { get = Getter<Record,_>(fun r -> r.B) ; set = Setter<_,_>(fun r v -> r.B <- v) }
    let tupleLens = { get = Getter<StructTupleType,_>(fun s -> let struct(_,_,_,_,_,_,_,_,_,x) = s in x) ; set = Setter<_,_>(fun s y -> let struct(x1,x2,x3,x4,x5,x6,x7,x8,x9,_) = s in s <- (x1,x2,x3,x4,x5,x6,x7,x8,x9,y)) }
    let unionTagReader = Getter<Union,_>(fun u -> u.GetTag_NoInlining())

    // TypeShape member accessors
    let recordShape =
        match shapeof<Record> with
        | Shape.FSharpRecord (:? ShapeFSharpRecord<Record> as shape) -> shape
        | _ -> failwith "should not fail"

    let stringMemberShape = recordShape.Fields.[0] :?> ShapeMember<Record, string>
    let intMemberShape = recordShape.Fields.[1] :?> ShapeMember<Record, int>

    let unionShape =
        match shapeof<Union> with
        | Shape.FSharpUnion (:? ShapeFSharpUnion<Union> as shape) -> shape
        | _ -> failwith "should not fail"

    let tupleShape =
        match shapeof<TupleType> with
        | Shape.Tuple (:? ShapeTuple<TupleType> as shape) -> shape
        | _ -> failwith "should not fail"

    let structTupleShape =
        match shapeof<StructTupleType> with
        | Shape.Tuple (:? ShapeTuple<StructTupleType> as shape) -> shape
        | _ -> failwith "should not fail"

    let tupleElemShape = structTupleShape.Elements.[0] :?> ShapeMember<StructTupleType, int>

    let mutable value = { A = "string" ; B = 42; C = false }
    let mutable tuple : StructTupleType = struct(1,1,1,1,1,1,1,1,1,1)
    
    let union1 = A 42
    let union2 = B("str", false)
    let union3 = C

    [<Benchmark>]
    member _.CreateRecord_BaseLine() = { A = defautl ; B = defautl ; C = defautl }
    [<Benchmark>]
    member _.CreateRecord_CustomLens() = defaultConstructor.Invoke()
    [<Benchmark>]
    member _.CreateRecord_TypeShape() = recordShape.CreateUninitialized()

    [<Benchmark>]
    member _.CreateTuple_BaseLine() : TupleType = (0,0,0,0,0,0,0,0,0,0)
    [<Benchmark>]
    member _.CreateTuple_CustomLens() = defaultTupleConstructor.Invoke()
    [<Benchmark>]
    member _.CreateTuple_TypeShape() = tupleShape.CreateUninitialized()

    [<Benchmark>]
    member _.CreateStructTuple_BaseLine() : StructTupleType = struct(0,0,0,0,0,0,0,0,0,0)
    [<Benchmark>]
    member _.CreateStructTuple_CustomLens() = defaultStructTupleConstructor.Invoke()
    [<Benchmark>]
    member _.CreateStructTuple_TypeShape() = structTupleShape.CreateUninitialized()

    [<Benchmark>]
    member _.GetString_BaseLine() = value.A
    [<Benchmark>]
    member _.GetString_CustomLens() = stringLens.get.Invoke &value
    [<Benchmark>]
    member _.GetString_TypeShape() = stringMemberShape.GetByRef &value

    [<Benchmark>]
    member _.SetString_BaseLine() = value.A <- "string"
    [<Benchmark>]
    member _.SetString_CustomLens() = stringLens.set.Invoke(&value, "string")
    [<Benchmark>]
    member _.SetString_TypeShape() = stringMemberShape.SetByRef(&value, "string")

    [<Benchmark>]
    member _.GetInt_BaseLine() = value.B
    [<Benchmark>]
    member _.GetInt_CustomLens() = intLens.get.Invoke &value
    [<Benchmark>]
    member _.GetInt_TypeShape() = intMemberShape.GetByRef &value

    [<Benchmark>]
    member _.SetInt_BaseLine() = value.B <- 42
    [<Benchmark>]
    member _.SetInt_CustomLens() = intLens.set.Invoke(&value, 42)
    [<Benchmark>]
    member _.SetInt_TypeShape() = intMemberShape.SetByRef(&value, 42)

    [<Benchmark>]
    member _.GetTuple_BaseLine() = let struct(_,_,_,_,_,_,_,_,_,x) = tuple in x
    [<Benchmark>]
    member _.GetTuple_CustomLens() = tupleLens.get.Invoke &tuple
    [<Benchmark>]
    member _.GetTuple_TypeShape() = tupleElemShape.GetByRef &tuple

    [<Benchmark>]
    member _.SetTuple_BaseLine() = let struct(x1,x2,x3,x4,x5,x6,x7,x8,x9,_) = tuple in (x1,x2,x3,x4,x5,x6,x7,x8,x9,42)
    [<Benchmark>]
    member _.SetTuple_CustomLens() = tupleLens.set.Invoke(&tuple, 42)
    [<Benchmark>]
    member _.SetTuple_TypeShape() = tupleElemShape.SetByRef(&tuple, 42)

    [<Benchmark(OperationsPerInvoke = 3)>]
    member _.GetUnionTag_BaseLine() =
        union1.GetTag() |> ignore
        union2.GetTag() |> ignore
        union3.GetTag() |> ignore

    [<Benchmark(OperationsPerInvoke = 3)>]
    member _.GetUnionTag_CustomLens() = 
        let utr = unionTagReader
        utr.Invoke &union1 |> ignore
        utr.Invoke &union2 |> ignore
        utr.Invoke &union3 |> ignore

    [<Benchmark(OperationsPerInvoke = 3)>]
    member _.GetUnionTag_TypeShape() =
        let us = unionShape
        us.GetTagByRef &union1 |> ignore
        us.GetTagByRef &union2 |> ignore
        us.GetTagByRef &union3 |> ignore