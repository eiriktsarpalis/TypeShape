# Performance

TypeShape is intended for real-world applications, and as such performance is a significant aspect of its design.
In this article I present a few benchmarks comparing generic programming approaches for common applications.

Unless otherwise stated, all benchmarks use [BenchmarkDotNet](https://benchmarkdotnet.org/) running .NET 6.0 on Windows:
```
BenchmarkDotNet=v0.13.1, OS=Windows 10.0.19043.1237 (21H1/May2021Update)
Intel Core i9-10900X CPU 3.70GHz, 1 CPU, 20 logical and 10 physical cores
.NET SDK=6.0.100-rc.2.21452.10
  [Host]     : .NET 6.0.0 (6.0.21.45117), X64 RyuJIT DEBUG
  Job-PUVFFK : .NET 6.0.0 (6.0.21.45117), X64 RyuJIT
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

|                      Method |         Mean |       Error |      StdDev |    Ratio | RatioSD |   Gen 0 | Allocated |
|---------------------------- |-------------:|------------:|------------:|---------:|--------:|--------:|----------:|
| &#39;FSharp.Core PrettyPrinter&#39; | 451,631.0 ns | 5,526.89 ns | 5,169.86 ns | 1,042.77 |   14.75 | 15.1367 |    153 KB |
|    &#39;Baseline PrettyPrinter&#39; |     433.0 ns |     2.93 ns |     2.60 ns |     1.00 |    0.00 |  0.1144 |      1 KB |
|   &#39;TypeShape PrettyPrinter&#39; |     787.8 ns |     5.51 ns |     4.60 ns |     1.82 |    0.01 |  0.1450 |      1 KB |

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

|                                     Method |     Mean |     Error |    StdDev | Ratio | RatioSD |  Gen 0 |  Gen 1 | Allocated |
|------------------------------------------- |---------:|----------:|----------:|------:|--------:|-------:|-------:|----------:|
|                          &#39;Baseline Cloner&#39; | 1.785 μs | 0.0092 μs | 0.0076 μs |  1.00 |    0.00 | 0.7305 | 0.0153 |      7 KB |
|                         &#39;TypeShape Cloner&#39; | 4.147 μs | 0.0489 μs | 0.0434 μs |  2.32 |    0.03 | 0.7706 | 0.0153 |      8 KB |
| &#39;TypeShape Staged Cloner with compilation&#39; | 1.967 μs | 0.0387 μs | 0.0475 μs |  1.11 |    0.03 | 0.7744 | 0.0153 |      8 KB |

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

|             Method |      Mean |    Error |   StdDev | Ratio | RatioSD |  Gen 0 | Allocated |
|------------------- |----------:|---------:|---------:|------:|--------:|-------:|----------:|
|   &#39;Baseline Empty&#39; |  39.05 ns | 0.777 ns | 0.926 ns |  1.00 |    0.00 | 0.0262 |     264 B |
| &#39;Reflection Empty&#39; | 303.26 ns | 4.212 ns | 3.517 ns |  7.78 |    0.20 | 0.0768 |     776 B |
|  &#39;TypeShape Empty&#39; | 177.29 ns | 3.276 ns | 2.904 ns |  4.56 |    0.16 | 0.0262 |     264 B |

## UnionContract

`UnionContract` is an implementation of a [contract pattern for schemaless datastores](https://eiriktsarpalis.wordpress.com/2018/10/30/a-contract-pattern-for-schemaless-datastores/).
It is used for encoding discriminated unions into data that can be easily embedded in common database storage formats.

For this benchmark we will be comparing the following implementations:

* [A bespoke union encoder for the type under test](https://github.com/eiriktsarpalis/TypeShape/blob/08a90e8a5bbb8fc1293037a10d0c6b8ef55c518e/tests/TypeShape.Benchmarks/UnionContract.fs#L36-L66),
* [A reflection-based generic union encoder](https://github.com/eiriktsarpalis/TypeShape/blob/08a90e8a5bbb8fc1293037a10d0c6b8ef55c518e/tests/TypeShape.Benchmarks/UnionContract.fs#L68-L87),
* [The TypeShape-based generic union encoder](https://github.com/eiriktsarpalis/TypeShape/blob/08a90e8a5bbb8fc1293037a10d0c6b8ef55c518e/src/TypeShape/Applications/UnionContract.fs).

### Results

|            Method |      Mean |     Error |    StdDev |  Gen 0 | Allocated |
|------------------ |----------:|----------:|----------:|-------:|----------:|
|   Encode_Baseline |  7.706 ns | 0.0431 ns | 0.0360 ns | 0.0034 |      34 B |
| Encode_Reflection | 27.385 ns | 0.1913 ns | 0.1696 ns | 0.0066 |      66 B |
|  Encode_TypeShape | 23.308 ns | 0.0941 ns | 0.0834 ns | 0.0034 |      34 B |
|   Decode_Baseline | 10.647 ns | 0.0867 ns | 0.0769 ns | 0.0031 |      31 B |
| Decode_Reflection | 90.961 ns | 0.1050 ns | 0.0877 ns | 0.0086 |      87 B |
|  Decode_TypeShape | 31.220 ns | 0.2973 ns | 0.2781 ns | 0.0031 |      31 B |

## Json Serialization

The TypeShape samples project implements a [JSON serializer](https://github.com/eiriktsarpalis/TypeShape/blob/main/samples/TypeShape.Samples/HKT/JsonSerializer.fs) based on the `Utf8JsonReader` and `Utf8JsonWriter` classes found in System.Text.Json.

Here are the results of a [benchmark](https://github.com/eiriktsarpalis/TypeShape/blob/main/tests/TypeShape.Benchmarks/JsonSerializer.fs) comparing the TypeShape serializer with the default serializer found in System.Text.Json:

|                     Method |     Mean |     Error |    StdDev |  Gen 0 | Allocated |
|--------------------------- |---------:|----------:|----------:|-------:|----------:|
|   Serialize_SystemTextJson | 2.109 μs | 0.0403 μs | 0.0464 μs | 0.1183 |      1 KB |
|        Serialize_TypeShape | 2.215 μs | 0.0218 μs | 0.0193 μs | 0.1297 |      1 KB |
| Deserialize_SystemTextJson | 3.780 μs | 0.0125 μs | 0.0111 μs | 0.1602 |      2 KB |
|      Deserialize_TypeShape | 3.821 μs | 0.0547 μs | 0.0511 μs | 0.2251 |      2 KB |

## FsPickler

FsPickler serialization is driven by TypeShape. Please see the relevant [performance page](https://github.com/mbraceproject/FsPickler/wiki/.NET-Core-Benchmarks) in that repo.
