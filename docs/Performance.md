# Performance

TypeShape is intended for real-world applications, and as such performance is a significant aspect of its design.
In this article I present a few benchmarks comparing generic programming approaches for common applications.

Unless otherwise stated, all benchmarks use [BenchmarkDotNet](https://benchmarkdotnet.org/) running .NET 5.0 on Linux:
```
BenchmarkDotNet=v0.12.1, OS=ubuntu 20.04
AMD EPYC 7452, 1 CPU, 4 logical and 2 physical cores
.NET Core SDK=5.0.100-rc.2.20479.15
  [Host]     : .NET Core 5.0.0 (CoreCLR 5.0.20.47505, CoreFX 5.0.20.47505), X64 RyuJIT DEBUG
  DefaultJob : .NET Core 5.0.0 (CoreCLR 5.0.20.47505, CoreFX 5.0.20.47505), X64 RyuJIT
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

|                      Method |         Mean |       Error |      StdDev |    Ratio | RatioSD |  Gen 0 | Gen 1 | Gen 2 | Allocated |
|---------------------------- |-------------:|------------:|------------:|---------:|--------:|-------:|------:|------:|----------:|
| &#39;FSharp.Core PrettyPrinter&#39; | 985,625.2 ns | 4,027.69 ns | 3,767.50 ns | 1,172.63 |    7.25 | 1.9531 |     - |     - | 156.42 KB |
|    &#39;Baseline PrettyPrinter&#39; |     841.2 ns |     4.41 ns |     3.68 ns |     1.00 |    0.00 | 0.0172 |     - |     - |   1.13 KB |
|   &#39;TypeShape PrettyPrinter&#39; |   3,364.0 ns |    21.32 ns |    17.81 ns |     4.00 |    0.03 | 0.0229 |     - |     - |   1.69 KB |

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
* [A Staged TypeShape cloner](https://github.com/eiriktsarpalis/TypeShape/blob/57845c26d55d2d0ac9b4a2ead47cee446dbd2db7/tests/TypeShape.Tests/StagedClone.fs) interpeted using [Unquote](https://github.com/SwensenSoftware/unquote),
* [A Staged TypeShape cloner](https://github.com/eiriktsarpalis/TypeShape/blob/57845c26d55d2d0ac9b4a2ead47cee446dbd2db7/tests/TypeShape.Tests/StagedClone.fs) compiled using [FSharp.Quotations.Evaluator](https://github.com/fsprojects/FSharp.Quotations.Evaluator).

### Results

|                             Method |         Mean |      Error |     StdDev |  Ratio | RatioSD |   Gen 0 | Gen 1 | Gen 2 | Allocated |
|----------------------------------- |-------------:|-----------:|-----------:|-------:|--------:|--------:|------:|------:|----------:|
|                  &#39;Baseline Cloner&#39; |     3.237 μs |  0.0312 μs |  0.0292 μs |   1.00 |    0.00 |  0.1106 |     - |     - |   7.24 KB |
|                 &#39;TypeShape Cloner&#39; |   252.932 μs |  3.0226 μs |  2.6795 μs |  78.14 |    1.14 |  1.4648 |     - |     - | 120.81 KB |
|  &#39;TypeShape Unquote Staged Cloner&#39; | 1,074.877 μs | 13.0047 μs | 12.1646 μs | 332.10 |    5.19 | 11.7188 |     - |     - | 778.44 KB |
| &#39;TypeShape Compiled Staged Cloner&#39; |     4.055 μs |  0.0307 μs |  0.0287 μs |   1.25 |    0.01 |  0.1144 |     - |     - |   7.62 KB |

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

|             Method |        Mean |     Error |    StdDev | Ratio | RatioSD |  Gen 0 | Gen 1 | Gen 2 | Allocated |
|------------------- |------------:|----------:|----------:|------:|--------:|-------:|------:|------:|----------:|
|   &#39;Baseline Empty&#39; |    83.05 ns |  0.396 ns |  0.351 ns |  1.00 |    0.00 | 0.0039 |     - |     - |     264 B |
| &#39;Reflection Empty&#39; | 1,487.92 ns |  8.743 ns |  8.179 ns | 17.91 |    0.13 | 0.0114 |     - |     - |     864 B |
|  &#39;TypeShape Empty&#39; | 3,138.24 ns | 23.273 ns | 20.631 ns | 37.79 |    0.24 | 0.0038 |     - |     - |     408 B |

## UnionContract

`UnionContract` is an implementation of a [contract pattern for schemaless datastores](https://eiriktsarpalis.wordpress.com/2018/10/30/a-contract-pattern-for-schemaless-datastores/). 
It is used for encoding discriminated unions into data that can be easily embedded in common database storage formats.

For this benchmark we will be comparing the following implementations:

* [A bespoke union encoder for the type under test](https://github.com/eiriktsarpalis/TypeShape/blob/08a90e8a5bbb8fc1293037a10d0c6b8ef55c518e/tests/TypeShape.Benchmarks/UnionContract.fs#L36-L66),
* [A reflection-based generic union encoder](https://github.com/eiriktsarpalis/TypeShape/blob/08a90e8a5bbb8fc1293037a10d0c6b8ef55c518e/tests/TypeShape.Benchmarks/UnionContract.fs#L68-L87),
* [The TypeShape-based generic union encoder](https://github.com/eiriktsarpalis/TypeShape/blob/08a90e8a5bbb8fc1293037a10d0c6b8ef55c518e/src/TypeShape/Applications/UnionContract.fs).

### Results

|                     Method |       Mean |    Error |   StdDev | Ratio | RatioSD |  Gen 0 | Gen 1 | Gen 2 | Allocated |
|--------------------------- |-----------:|---------:|---------:|------:|--------:|-------:|------:|------:|----------:|
|   &#39;Baseline Union Encoder&#39; |   686.7 ns |  7.78 ns |  7.27 ns |  1.00 |    0.00 | 0.0105 |     - |     - |     720 B |
| &#39;Reflection Union Encoder&#39; | 4,261.3 ns | 18.40 ns | 15.36 ns |  6.21 |    0.08 | 0.0229 |     - |     - |    1688 B |
|  &#39;TypeShape Union Encoder&#39; | 8,504.1 ns | 35.03 ns | 32.76 ns | 12.39 |    0.14 | 0.0153 |     - |     - |    1648 B |

## FsPickler

FsPickler serialization is driven by TypeShape. Please see the relevant [performance page](https://github.com/mbraceproject/FsPickler/wiki/.NET-Core-Benchmarks) in that repo.
