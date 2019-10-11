# Performance

TypeShape is a library intended for real-world applications, and as such performance is a significant aspect of its design.
In this article I present a few benchmarks comparing genering programming approaches for common applications.

Unless otherwise stated, all benchmarks use [BenchmarkDotNet](https://benchmarkdotnet.org/) running .NET Core 3.0 on Linux:
```
BenchmarkDotNet=v0.11.5, OS=ubuntu 18.04
Intel Xeon CPU E5-2673 v4 2.30GHz, 1 CPU, 4 logical and 2 physical cores
.NET Core SDK=3.0.100
[Host]     : .NET Core 3.0.0 (CoreCLR 4.700.19.46205, CoreFX 4.700.19.46214), 64bit RyuJIT DEBUG
DefaultJob : .NET Core 3.0.0 (CoreCLR 4.700.19.46205, CoreFX 4.700.19.46214), 64bit RyuJIT
```

Without further ado, here are the scenaria:

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
* [A pretty-printer written using TypeShape](https://github.com/eiriktsarpalis/TypeShape/blob/57845c26d55d2d0ac9b4a2ead47cee446dbd2db7/samples/TypeShape.Samples/HKT/PrettyPrinter.fs),
* [A bespoke pretty-printer for the type we need to print](https://github.com/eiriktsarpalis/TypeShape/blob/57845c26d55d2d0ac9b4a2ead47cee446dbd2db7/tests/TypeShape.Benchmarks/PrettyPrinter.fs#L25-L45).
  
### Results
  
|                      Method |         Mean |       Error |      StdDev |    Ratio | RatioSD |
|---------------------------- |-------------:|------------:|------------:|---------:|--------:|
| &#39;FSharp.Core PrettyPrinter&#39; | 2,449.132 us | 146.3810 us | 388.1817 us | 2,034.44 |  179.25 |
|    &#39;Baseline PrettyPrinter&#39; |     1.202 us |   0.0237 us |   0.0439 us |     1.00 |    0.00 |
|   &#39;TypeShape PrettyPrinter&#39; |     2.182 us |   0.0423 us |   0.0416 us |     1.82 |    0.09 |

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

|                             Method |        Mean |       Error |     StdDev |  Ratio | RatioSD |
|----------------------------------- |------------:|------------:|-----------:|-------:|--------:|
|                  &#39;Baseline Cloner&#39; |    12.92 us |   0.8312 us |   2.289 us |   1.00 |    0.00 |
|                 &#39;TypeShape Cloner&#39; |   144.48 us |  11.3463 us |  32.372 us |  11.58 |    3.82 |
|  &#39;TypeShape Unquote Staged Cloner&#39; | 1,548.06 us | 158.5485 us | 464.995 us | 126.61 |   38.88 |
| &#39;TypeShape Compiled Staged Cloner&#39; |    22.10 us |   2.6079 us |   7.313 us |   1.77 |    0.73 |

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
* [A bespoke implementation of `empty` for the tested type](https://github.com/eiriktsarpalis/TypeShape/blob/57845c26d55d2d0ac9b4a2ead47cee446dbd2db7/tests/TypeShape.Benchmarks/Empty.fs#L16-L17),
* [The standard TypeShape implementation of `empty`](https://github.com/eiriktsarpalis/TypeShape/blob/57845c26d55d2d0ac9b4a2ead47cee446dbd2db7/src/TypeShape/Applications/Empty.fs).

### Results

|            Method |     Mean |     Error |    StdDev | Ratio | RatioSD |
|------------------ |---------:|----------:|----------:|------:|--------:|
|  &#39;Baseline Empty&#39; | 113.8 ns |  2.670 ns |  7.872 ns |  1.00 |    0.00 |
| &#39;TypeShape Empty&#39; | 856.5 ns | 16.596 ns | 17.043 ns |  7.55 |    0.56 |
