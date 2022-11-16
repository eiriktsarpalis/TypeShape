# Performance

TypeShape is intended for real-world applications, and as such performance is a significant aspect of its design.
In this article I present a few benchmarks comparing generic programming approaches for common applications.

Unless otherwise stated, all benchmarks use [BenchmarkDotNet](https://benchmarkdotnet.org/) running .NET 7.0 on MacOS:
```
BenchmarkDotNet=v0.13.2, OS=macOS 13.0 (22A380) [Darwin 22.1.0]
Apple M1, 1 CPU, 8 logical and 8 physical cores
.NET SDK=7.0.100
  [Host]     : .NET 7.0.0 (7.0.22.51805), Arm64 RyuJIT AdvSIMD DEBUG
  Job-GAGGLY : .NET 7.0.0 (7.0.22.51805), Arm64 RyuJIT AdvSIMD
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
|    &#39;Baseline PrettyPrinter&#39; |     320.4 ns |   2.18 ns |   2.04 ns |   1.00 |    0.00 | 0.0224 |   1.13 KB |        1.00 |
| &#39;FSharp.Core PrettyPrinter&#39; | 296,448.9 ns | 519.21 ns | 485.67 ns | 925.30 |    5.41 | 2.9297 | 153.44 KB |      136.39 |
|   &#39;TypeShape PrettyPrinter&#39; |     638.4 ns |   2.98 ns |   2.64 ns |   1.99 |    0.02 | 0.0286 |   1.43 KB |        1.27 |

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
|                          &#39;Baseline Cloner&#39; | 1.783 μs | 0.0052 μs | 0.0046 μs |  1.00 | 0.1431 | 0.0019 |    7.2 KB |        1.00 |
|                         &#39;TypeShape Cloner&#39; | 4.845 μs | 0.0143 μs | 0.0126 μs |  2.72 | 0.1450 |      - |   7.62 KB |        1.06 |
| &#39;TypeShape Staged Cloner with compilation&#39; | 1.955 μs | 0.0064 μs | 0.0060 μs |  1.10 | 0.1526 |      - |   7.62 KB |        1.06 |

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
|   &#39;Baseline Empty&#39; |  43.75 ns | 0.157 ns | 0.147 ns |  1.00 |    0.00 | 0.0051 |     264 B |        1.00 |
| &#39;Reflection Empty&#39; | 256.22 ns | 1.059 ns | 0.884 ns |  5.85 |    0.02 | 0.0148 |     776 B |        2.94 |
|  &#39;TypeShape Empty&#39; | 154.97 ns | 0.334 ns | 0.279 ns |  3.54 |    0.02 | 0.0050 |     264 B |        1.00 |

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
|   Encode_Baseline |  9.010 ns | 0.0542 ns | 0.0480 ns | 0.0007 |      34 B |
| Encode_Reflection | 26.429 ns | 0.1372 ns | 0.1284 ns | 0.0013 |      66 B |
|  Encode_TypeShape | 16.951 ns | 0.0811 ns | 0.0759 ns | 0.0007 |      34 B |
|   Decode_Baseline |  9.135 ns | 0.0240 ns | 0.0213 ns | 0.0006 |      31 B |
| Decode_Reflection | 74.797 ns | 0.3100 ns | 0.2748 ns | 0.0016 |      87 B |
|  Decode_TypeShape | 28.600 ns | 0.0831 ns | 0.0777 ns | 0.0006 |      31 B |

## Json Serialization

The TypeShape samples project implements a [JSON serializer](https://github.com/eiriktsarpalis/TypeShape/blob/main/samples/TypeShape.Samples/HKT/JsonSerializer.fs) based on the `Utf8JsonReader` and `Utf8JsonWriter` classes found in System.Text.Json.

Here are the results of a [benchmark](https://github.com/eiriktsarpalis/TypeShape/blob/main/tests/TypeShape.Benchmarks/JsonSerializer.fs) comparing the TypeShape serializer with the default serializer found in System.Text.Json:

|                     Method |     Mean |     Error |    StdDev |   Gen0 | Allocated |
|--------------------------- |---------:|----------:|----------:|-------:|----------:|
|   Serialize_SystemTextJson | 1.242 us | 0.0021 us | 0.0018 us | 0.0191 |      1 KB |
|        Serialize_TypeShape | 1.058 us | 0.0020 us | 0.0018 us | 0.0229 |   1.19 KB |
| Deserialize_SystemTextJson | 2.424 us | 0.0068 us | 0.0064 us | 0.0305 |   1.65 KB |
|      Deserialize_TypeShape | 2.428 us | 0.0180 us | 0.0169 us | 0.0420 |   2.15 KB |

## FsPickler

FsPickler serialization is driven by TypeShape. Please see the relevant [performance page](https://github.com/mbraceproject/FsPickler/wiki/.NET-Core-Benchmarks) in that repo.
