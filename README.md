# TypeShape

TypeShape is a small, extensible F# library for practical generic programming.
Borrowing from ideas used in the FsPickler [implementation](http://mbraceproject.github.io/FsPickler/overview.html#Pickler-Generation),
it uses a combination of reflection, active patterns and F# object expressions to minimize the
amount of reflection required by the user in such applications.

TypeShape permits definition of programs that act on specific algebrae of types.
The library uses reflection to derive the algebraic structure of a given
`System.Type` instance and then applies a variant of the visitor pattern
to provide relevant type information per shape.

See my [slides](http://eiriktsarpalis.github.io/typeshape/) for a more thorough introduction to the concept.

### Installing

To incorporate TypeShape in your project place the following line in your
`paket.dependencies` file:
```
github eiriktsarpalis/TypeShape:2.10 src/TypeShape/TypeShape.fs
```
and in `paket.references`:
```
File: TypeShape.fs TypeShape
```
TypeShape is also available on [![NuGet Status](http://img.shields.io/nuget/v/TypeShape.svg?style=flat)](https://www.nuget.org/packages/TypeShape/)

### Example: Implementing a value printer

```fsharp
open System
open TypeShape

let rec mkPrinter<'T> () : 'T -> string =
    let wrap(p : 'a -> string) = unbox<'T -> string> p
    match shapeof<'T> with
    | Shape.Unit -> wrap(fun () -> "()")
    | Shape.Bool -> wrap(sprintf "%b")
    | Shape.Byte -> wrap(fun (b:byte) -> sprintf "%duy" b)
    | Shape.Int32 -> wrap(sprintf "%d")
    | Shape.Int64 -> wrap(fun (b:int64) -> sprintf "%dL" b)
    | Shape.String -> wrap(sprintf "\"%s\"")
    | Shape.FSharpOption s ->
        s.Element.Accept {
            new ITypeVisitor<'T -> string> with
                member __.Visit<'a> () =
                    let tp = mkPrinter<'a>()
                    wrap(function None -> "None" | Some t -> sprintf "Some (%s)" (tp t))
        }

    | Shape.FSharpList s ->
        s.Element.Accept {
            new ITypeVisitor<'T -> string> with
                member __.Visit<'a> () =
                    let tp = mkPrinter<'a>()
                    wrap(fun ts -> ts |> List.map tp |> String.concat "; " |> sprintf "[%s]")
        }

    | Shape.Array s when s.Rank = 1 ->
        s.Element.Accept {
            new ITypeVisitor<'T -> string> with
                member __.Visit<'a> () =
                    let tp = mkPrinter<'a> ()
                    wrap(fun ts -> ts |> Array.map tp |> String.concat "; " |> sprintf "[|%s|]")
        }
        
    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
        let mkElemPrinter (shape : IShapeMember<'T>) =
           shape.Accept { new IMemberVisitor<'T, 'T -> string> with
               member __.Visit (shape : ShapeMember<'DeclaringType, 'Field>) =
                   let fieldPrinter = mkPrinter<'Field>()
                   fieldPrinter << shape.Get }

        let elemPrinters : ('T -> string) [] = s.Fields |> Array.map mkElemPrinter

        fun (r:'T) ->
            elemPrinters
            |> Seq.map (fun ep -> ep r)
            |> String.concat ", "
            |> sprintf "(%s)"

    | Shape.FSharpSet s ->
        s.Accept {
            new IFSharpSetVisitor<'T -> string> with
                member __.Visit<'a when 'a : comparison> () =
                    let tp = mkPrinter<'a>()
                    wrap(fun (s:Set<'a>) -> s |> Seq.map tp |> String.concat "; " |> sprintf "set [%s]")
        }

    | _ -> failwithf "unsupported type '%O'" typeof<'T>

let p = mkPrinter<int * bool option * string list * int []> ()
p (42, Some false, ["string"], [|1;2;3;4;5|])
```
Let's see how the value printer compares to sprintf:
```fsharp
#time "on"

type TestType = (int list * string option * string) * (bool * unit)
let value : TestType = (([1 .. 5], None, "42"), (false, ()))

let p1 = sprintf "%A" : TestType -> string
let p2 = mkPrinter<TestType>()

// Real: 00:00:00.442, CPU: 00:00:00.437, GC gen0: 31, gen1: 0, gen2: 0
for i = 1 to 1000 do ignore <| p1 value
// Real: 00:00:00.006, CPU: 00:00:00.000, GC gen0: 2, gen1: 1, gen2: 0
for i = 1 to 1000 do ignore <| p2 value
```

### Records, Unions and POCOs

TypeShape can be used to define generic programs that access fields of arbitrary types:
F# records, unions or POCOs. This is achieved using the `IShapeMember` abstraction:
```fsharp
type IShapeMember<'DeclaringType, 'Field> =
    abstract Get : 'DeclaringType -> 'Field
    abstract Set : 'DeclaringType -> 'Field -> 'DeclaringType
```
An F# record then is just a list of member shapes, a union is a list of lists of member shapes.
Member shapes can optionally be configured to generate code at runtime for more performant `Get` and `Set` operations.
Member shapes come with quoted versions of the API for staged generic programming applications.

To make our pretty printer support these types, we first provide a pretty printer for members:
```fsharp
let mkMemberPrinter (shape : IShapeMember<'DeclaringType>) =
   shape.Accept { new IMemberVisitor<'DeclaringType, 'DeclaringType -> string> with
       member __.Visit (shape : ShapeMember<'DeclaringType, 'Field>) =
           let fieldPrinter = mkPrinter<'Field>()
           fieldPrinter << shape.Get }
```
Then for F# records:
```fsharp
    match shapeof<'T> with
    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
        let fieldPrinters : (string * ('T -> string)) [] = 
            s.Fields |> Array.map (fun f -> f.Label, mkMemberPrinter f)

        fun (r:'T) ->
            fieldPrinters
            |> Seq.map (fun (label, fp) -> sprintf "%s = %s" label (fp r))
            |> String.concat "; "
            |> sprintf "{ %s }"
```
Similarly, we could also add support for arbitrary F# unions:
```fsharp
    match shapeof<'T> with
    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
        let cases : ShapeFSharpUnionCase<'T> [] = shape.UnionCases // all union cases
        let mkUnionCasePrinter (case : ShapeFSharpUnionCase<'T>) =
            let fieldPrinters = case.Fields |> Array.map mkMemberPrinter
            fun (u:'T) -> 
                fieldPrinters 
                |> Seq.map (fun fp -> fp u) 
                |> String.concat ", "
                |> sprintf "%s(%s)" case.CaseInfo.Name

        let casePrinters = cases |> Array.map mkUnionCasePrinter // generate printers for all union cases
        fun (u:'T) ->
            let tag : int = shape.GetTag u // get the underlying tag for the union case
            casePrinters.[tag] u
```
Similar active patterns exist for classes with settable properties and general POCOs.

### Extensibility

TypeShape can be extended to incorporate new active patterns supporting arbitrary shapes.
Here's an [example](https://github.com/eiriktsarpalis/TypeShape/blob/5dabaf0577d8387c5213a496099598bbd89650b8/src/TypeShape/ISerializableExtensions.fs) 
illustrating how TypeShape can be extended to support ISerializable shapes.

### Additional examples

See the project [samples](https://github.com/eiriktsarpalis/TypeShape/tree/master/samples) folder for more implementations using TypeShape:

* [Printer.fs](samples/TypeShape.Samples/printer.fs) Pretty printer generator for common F# types.
* [Parser.fs](samples/TypeShape.Samples/parser.fs) Parser generator for common F# types using FParsec.
* [Equality-Comparer.fs](samples/TypeShape.Samples/equality-comparer.fs) Equality comparer generator for common F# types.
* [Gmap.fsx](https://github.com/eiriktsarpalis/TypeShape/blob/bdf56d8519c288d1029a41d088d5896ee159c970/samples/gmap.fsx) A gmap implementation.
* [hashcode-staged.fs](samples/TypeShape.Samples/hashcode-staged.fs) Staged generic hashcode generator.

### Projects using TypeShape

* [FsPickler](https://github.com/mbraceproject/FsPickler/blob/c5b73fc48fa313c66eaeb8c79897253de0605d34/src/FsPickler/PicklerGeneration/PicklerGenerator.fs#L38)
* [FSharp.AWS.DynamoDB](https://github.com/fsprojects/FSharp.AWS.DynamoDB/blob/55470a0cc8b1a54d14571f059bd2b5721f2495c7/src/FSharp.AWS.DynamoDB/Picklers/PicklerResolver.fs#L23)
* [Logary](https://github.com/logary/logary)
* [Equinox](https://github.com/jet/equinox)
* [FsConfig](https://github.com/demystifyfp/FsConfig)

### Related Work

* [DataType Generic Programming in F#](http://www.staff.science.uu.nl/~swier004/publications/2015-wgp.pdf), 
* [Infers](https://github.com/Infers/Infers)

### Build Status

Head (branch `master`), Build & Unit tests

* Windows [![Build status](https://ci.appveyor.com/api/projects/status/6t6vovc2xrj8nqh9/branch/master?svg=true)](https://ci.appveyor.com/project/nessos/typeshape/branch/master)
* Linux [![Build Status](https://api.travis-ci.org/eiriktsarpalis/TypeShape.svg?branch=master)](https://travis-ci.org/eiriktsarpalis/TypeShape/branches)
