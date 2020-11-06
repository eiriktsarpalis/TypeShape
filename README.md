# TypeShape [![Build & Tests](https://github.com/eiriktsarpalis/TypeShape/workflows/Build%20&%20Tests/badge.svg)](https://github.com/eiriktsarpalis/TypeShape/actions?query=branch%3Amaster) [![NuGet Badge](https://buildstats.info/nuget/TypeShape)](https://www.nuget.org/packages/TypeShape/)

TypeShape is a small, extensible F# library for practical datatype-generic programming.
Borrowing from ideas used in the FsPickler [implementation](http://mbraceproject.github.io/FsPickler/overview.html#Pickler-Generation),
it uses a combination of reflection, active patterns and F# object expressions to minimize the
amount of reflection required by the user in such applications.

TypeShape permits definition of programs that act on specific algebrae of types.
The library uses reflection to derive the algebraic structure of a given
`System.Type` instance and then applies a variant of the visitor pattern
to provide relevant type information per shape.

TypeShape can provide significant performance improvements compared to equivalent reflection-based approaches.
See the [performance page](https://github.com/eiriktsarpalis/TypeShape/blob/master/docs/Performance.md) for more details and benchmarks.

Please see my [article](https://eiriktsarpalis.wordpress.com/2016/08/05/typeshape-practical-generic-programming-in-f/) and [slides](http://eiriktsarpalis.github.io/typeshape/) for a more thorough introduction to the concept.

### Installing

To incorporate TypeShape in your project place the following line in your
`paket.dependencies` file:
```
github eiriktsarpalis/TypeShape:8.0 src/TypeShape/TypeShape.fs
```
and in `paket.references`:
```
File: TypeShape.fs TypeShape
```
TypeShape is also available on [![NuGet Badge](https://buildstats.info/nuget/TypeShape)](https://www.nuget.org/packages/TypeShape/)

### Example: Implementing a value printer

```fsharp
open System
open TypeShape.Core

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

        let elemPrinters : ('T -> string) [] = shape.Elements |> Array.map mkElemPrinter

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
// val it : string = "(42, Some (false), ["string"], [|1; 2; 3; 4; 5|])"
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
* [hashcode-staged.fs](samples/TypeShape.Samples/hashcode-staged.fs) Staged generic hashcode generator.
* [Gmap](https://github.com/eiriktsarpalis/TypeShape/blob/master/src/TypeShape/Applications/Combinators.fs#L304) There are set of `gmap` related functions within the `TypeShape.Generic` module in the Nuget package. 

### Using the Higher-Kinded Type API (Experimental)

As of [TypeShape 8](https://github.com/eiriktsarpalis/TypeShape/releases/tag/8.0.0) it is possible to avail of an experimental higher-kinded type flavour of the api,
which can be used to author fully type-safe programs for most common applications.
Please see [my original article](https://eiriktsarpalis.wordpress.com/2019/07/02/applying-the-tagless-final-pattern-in-f-generic-programs/) on the subject for background and motivation.

To use the new approach, we first need to specify which types we would like our generic program to support:

```fsharp
open TypeShape.HKT

type IMyTypesBuilder<'F> =
    inherit IBoolBuilder<'F>
    inherit IInt32Builder<'F>
    inherit IStringBuilder<'F>

    inherit IFSharpOptionBuilder<'F>
    inherit IFSharpListBuilder<'F>
    inherit ITuple2Builder<'F>
```

The interface `MyTypeBuilder<'F>` denotes a "higher-kinded" generic program builder
which supports combinations of boolean, integer, string, optional, list and pair types. 

Next, we need to define how interface implementations are to be folded:

```fsharp
let mkGenericProgram (builder : IMyTypesBuilder<'F>) =
    { new IGenericProgram<'F> with
        member this.Resolve<'a> () : App<'F, 'a> = 
            match shapeof<'a> with
            | Fold.Bool builder r -> r
            | Fold.Int32 builder r -> r
            | Fold.String builder r -> r
            | Fold.Tuple2 builder this r -> r
            | Fold.FSharpOption builder this r -> r
            | Fold.FSharpList builder this r -> r
            | _ -> failwithf "I do not know how to fold type %O" typeof<'a> }
```

This piece of boilerplate composes built-in `Fold.*` active patterns,
which contain folding logic for the individual builders inherited by the interface.
Note that the order of composition can be significant (e.g. folding with `FSharpOption` before `FSharpUnion`).

Let's now provide a pretty-printer implementation for our interface:

```fsharp
// Higher-Kinded encoding
type PrettyPrinter =
    static member Assign(_ : App<PrettyPrinter, 'a>, _ : 'a -> string) = ()

// Implementing the interface
let prettyPrinterBuilder =
    { new IMyTypesBuilder<PrettyPrinter> with
        member __.Bool () = HKT.pack (function false -> "false" | true -> "true")
        member __.Int32 () = HKT.pack (sprintf "%d")
        member __.String () = HKT.pack (sprintf "\"%s\"")

        member __.Option (HKT.Unpack elemPrinter) = HKT.pack(function None -> "None" | Some a -> sprintf "Some(%s)" (elemPrinter a))
        member __.Tuple2 (HKT.Unpack left) (HKT.Unpack right) = HKT.pack(fun (a,b) -> sprintf "(%s, %s)" (left a) (right b))
        member __.List (HKT.Unpack elemPrinter) = HKT.pack(Seq.map elemPrinter >> String.concat "; " >> sprintf "[%s]") }
```

Putting it all together gives us a working pretty-printer:

```fsharp
let prettyPrint<'t> : 't -> string = (mkGenericProgram prettyPrinterBuilder).Resolve<'t> () |> HKT.unpack

prettyPrint 42
prettyPrint (Some false)
prettyPrint (Some "test", [Some 42; None; Some -1])
```

Please check the [samples/HKT](https://github.com/eiriktsarpalis/TypeShape/tree/23dabf9e863464c08a3ca82ad452695615f62708/samples/TypeShape.Samples/HKT) folder for real-world examples of the above.

### Projects using TypeShape

* [FsPickler](https://github.com/mbraceproject/FsPickler/blob/c5b73fc48fa313c66eaeb8c79897253de0605d34/src/FsPickler/PicklerGeneration/PicklerGenerator.fs#L38)
* [FSharp.AWS.DynamoDB](https://github.com/fsprojects/FSharp.AWS.DynamoDB/blob/55470a0cc8b1a54d14571f059bd2b5721f2495c7/src/FSharp.AWS.DynamoDB/Picklers/PicklerResolver.fs#L23)
* [Logary](https://github.com/logary/logary)
* [Equinox/FsCodec](https://github.com/jet/FsCodec)
* [FsConfig](https://github.com/demystifyfp/FsConfig)

### Related Work

* [DataType Generic Programming in F#](http://www.staff.science.uu.nl/~swier004/publications/2015-wgp.pdf), 
* [Infers](https://github.com/Infers/Infers)
