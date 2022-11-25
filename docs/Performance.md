# Performance

TypeShape is intended for real-world applications, and as such performance is a significant aspect of its design.
In this article I present a few benchmarks comparing generic programming approaches for common applications.

Unless otherwise stated, all benchmarks use [BenchmarkDotNet](https://benchmarkdotnet.org/) running .NET 7.0 on MacOS:
```
BenchmarkDotNet=v0.13.2, OS=macOS 13.0.1 (22A400) [Darwin 22.1.0]
Apple M1, 1 CPU, 8 logical and 8 physical cores
.NET SDK=7.0.100
  [Host]     : .NET 7.0.0 (7.0.22.51805), Arm64 RyuJIT AdvSIMD DEBUG
  Job-CXMRZS : .NET 7.0.0 (7.0.22.51805), Arm64 RyuJIT AdvSIMD
```
You can find the benchmarks project [here](https://github.com/eiriktsarpalis/TypeShape/tree/master/tests/TypeShape.Benchmarks).
Without further ado, here are the results:

## Pretty-Printer

Given a standard F# value, (combination of primitives, tuples, records and unions)
generate a string rendering the value in F# syntax.

We'll be testing the following value:
```fsharp
let testValue : TestType = 
    struct(
        [  [{ A = "value" ; B = 42 ; C = false }]; []; [{ A = "A'" ; B = 0 ; C = true }] ],
        [| [] ; ["A";"B"] |], 
        [| A 42; B; B ; C("value", 0) |])
```

The following implementations were benchmarked:

* `sprintf "%A"`: the standard F# core pretty-printer,
* [A bespoke pretty-printer for the type we need to print](https://github.com/eiriktsarpalis/TypeShape/blob/57845c26d55d2d0ac9b4a2ead47cee446dbd2db7/tests/TypeShape.Benchmarks/PrettyPrinter.fs#L25-L85),
* [A pretty-printer written using TypeShape](https://github.com/eiriktsarpalis/TypeShape/blob/57845c26d55d2d0ac9b4a2ead47cee446dbd2db7/samples/TypeShape.Samples/HKT/PrettyPrinter.fs).
  
### Results

|                      Method |         Mean |     Error |    StdDev |  Ratio | RatioSD |   Gen0 | Allocated | Alloc Ratio |
|---------------------------- |-------------:|----------:|----------:|-------:|--------:|-------:|----------:|------------:|
|    &#39;Baseline PrettyPrinter&#39; |     319.5 ns |   1.28 ns |   1.20 ns |   1.00 |    0.00 | 0.0224 |   1.13 KB |        1.00 |
| &#39;FSharp.Core PrettyPrinter&#39; | 292,729.5 ns | 402.35 ns | 376.36 ns | 916.33 |    3.45 | 2.9297 | 153.44 KB |      136.39 |
|   &#39;TypeShape PrettyPrinter&#39; |     638.8 ns |   2.19 ns |   2.05 ns |   2.00 |    0.01 | 0.0286 |   1.43 KB |        1.27 |

The bespoke implementation is twice as fast as the TypeShape program,
however it is still significantly faster than the default core implementation.

## Value Cloner

Given a standard F# value, (combination of primitives, tuples, records and unions)
create an equal value whose object graph is completely disconnected.
This somewhat artificial example is a good benchmark since it exercises most of the
TypeShape surface API (it both reads and creates values).

We'll be cloning the following value:
```fsharp
let testValue : TestType =
    let rs = [ for i in 1 .. 20 -> { A = sprintf "lorem ipsum %d" i ; B = i ; C = i % 2 = 0 } ]
    let ss = [for i in 1 .. 100 -> string i]
    struct([rs; []], [|ss|], [|1 .. 20|])
```

The following implementations were benchmarked:
* [A bespoke cloner for the tested type](https://github.com/eiriktsarpalis/TypeShape/blob/57845c26d55d2d0ac9b4a2ead47cee446dbd2db7/tests/TypeShape.Benchmarks/Clone.fs#L14-L24),
* [A TypeShape cloner using the standard API](https://github.com/eiriktsarpalis/TypeShape/blob/57845c26d55d2d0ac9b4a2ead47cee446dbd2db7/src/TypeShape/Applications/Clone.fs),
* [A Staged TypeShape cloner](https://github.com/eiriktsarpalis/TypeShape/blob/57845c26d55d2d0ac9b4a2ead47cee446dbd2db7/tests/TypeShape.Tests/StagedClone.fs) compiled using [FSharp.Quotations.Evaluator](https://github.com/fsprojects/FSharp.Quotations.Evaluator).

### Results

|                                     Method |     Mean |     Error |    StdDev | Ratio |   Gen0 |   Gen1 | Allocated | Alloc Ratio |
|------------------------------------------- |---------:|----------:|----------:|------:|-------:|-------:|----------:|------------:|
|                          &#39;Baseline Cloner&#39; | 1.749 μs | 0.0046 μs | 0.0043 μs |  1.00 | 0.1431 | 0.0019 |    7.2 KB |        1.00 |
|                         &#39;TypeShape Cloner&#39; | 4.781 μs | 0.0151 μs | 0.0141 μs |  2.73 | 0.1450 |      - |   7.62 KB |        1.06 |
| &#39;TypeShape Staged Cloner with compilation&#39; | 1.900 μs | 0.0267 μs | 0.0250 μs |  1.09 | 0.1526 |      - |   7.62 KB |        1.06 |

The standard TypeShape cloner is an order of magnitude slower than the bespoke implementation,
however the compiled staged cloner offers very comparable performance.
It certainly demonstrates the promise of [staged generic programming](http://fssnip.azurewebsites.net/7Ry/title/Staged-Generic-Equality)
[applications](http://fssnip.azurewebsites.net/7Rz/title/Staged-Generic-Hashcodes).

## Empty

The `empty<'T> : 'T` function is a standard utility included in the TypeShape library which
builds "zero" values for standard F# types. For example, the expression
```fsharp
empty<{| x : int ; y : {| z : int option ; w : string |} |}>
```
returns an instance of the supplied type argument with non-null, unpopulated fields:
```fsharp
val it : {| x: int; y: {| y: string; z: int option |} |} =
  { x = 0
    y = { y = ""
          z = None } }
```
It can be thought of as safe version of `Unchecked.defaultof<'T>` which can be useful for mocking values in tests:
```fsharp
{ empty<HugeDomainRecord> with InterestingValue1 = "x" ; InterstingValue2 = 42 }
```

For the purposes of this benchmark, we'll be comparing:
* [A bespoke implementation of `empty` for the tested type](https://github.com/eiriktsarpalis/TypeShape/blob/08a90e8a5bbb8fc1293037a10d0c6b8ef55c518e/tests/TypeShape.Benchmarks/Empty.fs#L20-L21),
* [A reflection-based generic implementation of `empty`](https://github.com/eiriktsarpalis/TypeShape/blob/08a90e8a5bbb8fc1293037a10d0c6b8ef55c518e/tests/TypeShape.Benchmarks/Empty.fs#L23-L54),
* [The standard TypeShape implementation of `empty`](https://github.com/eiriktsarpalis/TypeShape/blob/08a90e8a5bbb8fc1293037a10d0c6b8ef55c518e/src/TypeShape/Applications/Empty.fs).

### Results

|             Method |      Mean |    Error |   StdDev | Ratio | RatioSD |   Gen0 | Allocated | Alloc Ratio |
|------------------- |----------:|---------:|---------:|------:|--------:|-------:|----------:|------------:|
|   &#39;Baseline Empty&#39; |  42.88 ns | 0.247 ns | 0.231 ns |  1.00 |    0.00 | 0.0051 |     264 B |        1.00 |
| &#39;Reflection Empty&#39; | 252.56 ns | 0.730 ns | 0.683 ns |  5.89 |    0.04 | 0.0148 |     776 B |        2.94 |
|  &#39;TypeShape Empty&#39; | 153.09 ns | 0.474 ns | 0.443 ns |  3.57 |    0.02 | 0.0050 |     264 B |        1.00 |

## UnionContract

`UnionContract` is an implementation of a [contract pattern for schemaless datastores](https://eiriktsarpalis.wordpress.com/2018/10/30/a-contract-pattern-for-schemaless-datastores/).
It is used for encoding discriminated unions into data that can be easily embedded in common database storage formats.

For this benchmark we will be comparing the following implementations:

* [A bespoke union encoder for the type under test](https://github.com/eiriktsarpalis/TypeShape/blob/08a90e8a5bbb8fc1293037a10d0c6b8ef55c518e/tests/TypeShape.Benchmarks/UnionContract.fs#L36-L66),
* [A reflection-based generic union encoder](https://github.com/eiriktsarpalis/TypeShape/blob/08a90e8a5bbb8fc1293037a10d0c6b8ef55c518e/tests/TypeShape.Benchmarks/UnionContract.fs#L68-L87),
* [The TypeShape-based generic union encoder](https://github.com/eiriktsarpalis/TypeShape/blob/08a90e8a5bbb8fc1293037a10d0c6b8ef55c518e/src/TypeShape/Applications/UnionContract.fs).

### Results

|            Method |      Mean |     Error |    StdDev |   Gen0 | Allocated |
|------------------ |----------:|----------:|----------:|-------:|----------:|
|   Encode_Baseline | 10.242 ns | 0.0267 ns | 0.0237 ns | 0.0007 |      34 B |
| Encode_Reflection | 26.203 ns | 0.0288 ns | 0.0241 ns | 0.0013 |      66 B |
|  Encode_TypeShape | 16.476 ns | 0.0161 ns | 0.0143 ns | 0.0007 |      34 B |
|   Decode_Baseline |  9.161 ns | 0.0175 ns | 0.0164 ns | 0.0006 |      31 B |
| Decode_Reflection | 74.727 ns | 0.2632 ns | 0.2462 ns | 0.0016 |      87 B |
|  Decode_TypeShape | 28.228 ns | 0.2214 ns | 0.2071 ns | 0.0006 |      31 B |

## Json Serialization

The TypeShape samples project implements a [JSON serializer](https://github.com/eiriktsarpalis/TypeShape/blob/main/samples/TypeShape.Samples/HKT/JsonSerializer.fs) based on the `Utf8JsonReader` and `Utf8JsonWriter` classes found in System.Text.Json.

Here are the results of a [benchmark](https://github.com/eiriktsarpalis/TypeShape/blob/main/tests/TypeShape.Benchmarks/JsonSerializer.fs) comparing the TypeShape serializer with the default serializer found in System.Text.Json:

|                     Method |     Mean |     Error |    StdDev |   Gen0 | Allocated |
|--------------------------- |---------:|----------:|----------:|-------:|----------:|
|   Serialize_SystemTextJson | 1.243 μs | 0.0055 μs | 0.0051 μs | 0.0191 |      1 KB |
|        Serialize_TypeShape | 1.069 μs | 0.0037 μs | 0.0035 μs | 0.0229 |   1.19 KB |
| Deserialize_SystemTextJson | 2.388 μs | 0.0114 μs | 0.0107 μs | 0.0305 |   1.65 KB |
|      Deserialize_TypeShape | 2.275 μs | 0.0073 μs | 0.0068 μs | 0.0267 |   1.45 KB |

## FsPickler

FsPickler serialization is driven by TypeShape. Please see the relevant [performance page](https://github.com/mbraceproject/FsPickler/wiki/.NET-Core-Benchmarks) in that repo.
