# TypeShape

TypeShape is a small, extensible F# library for practical generic programming.
Borrowing from ideas used in the FsPickler [implementation](http://mbraceproject.github.io/FsPickler/overview.html#Pickler-Generation),
it uses a combination of reflection, active patterns and F# object expressions to minimize the
amount of reflection required by the user in such applications.

TypeShape permits definition of programs that act on specific algebrae of types.
The library uses reflection to derive the algebraic structure of a given
`System.Type` instance and then applies a variant of the visitor pattern
to provide relevant type information per shape.

### Installing

To incorporate TypeShape in your project place the following line in your
`paket.dependencies` file:
```
github eiriktsarpalis/TypeShape:2.0 src/TypeShape/TypeShape.fs
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
    match TypeShape.Create<'T>() with
    | Shape.Unit -> wrap(fun () -> "()")
    | Shape.Bool -> wrap(sprintf "%b")
    | Shape.Byte -> wrap(fun (b:byte) -> sprintf "%duy" b)
    | Shape.Int32 -> wrap(sprintf "%d")
    | Shape.Int64 -> wrap(fun (b:int64) -> sprintf "%dL" b)
    | Shape.String -> wrap(sprintf "\"%s\"")
    | Shape.FSharpOption s ->
        s.Accept {
            new IFSharpOptionVisitor<'T -> string> with
                member __.Visit<'a> () =
                    let tp = mkPrinter<'a>()
                    wrap(function None -> "None" | Some t -> sprintf "Some (%s)" (tp t))
        }

    | Shape.Tuple2 s ->
        s.Accept {
            new ITuple2Visitor<'T -> string> with
                member __.Visit<'t1, 't2> () =
                    let tp = mkPrinter<'t1>()
                    let sp = mkPrinter<'t2>()
                    wrap(fun (t : 't1, s : 't2) -> sprintf "(%s, %s)" (tp t) (sp s))
        }

    | Shape.FSharpList s ->
        s.Accept {
            new IFSharpListVisitor<'T -> string> with
                member __.Visit<'a> () =
                    let tp = mkPrinter<'a>()
                    wrap(fun ts -> ts |> List.map tp |> String.concat "; " |> sprintf "[%s]")
        }

    | Shape.Array s ->
        s.Accept {
            new IArrayVisitor<'T -> string> with
                member __.Visit<'a> () =
                    let tp = mkPrinter<'a> ()
                    wrap(fun ts -> ts |> Array.map tp |> String.concat "; " |> sprintf "[|%s|]")
        }

    | Shape.FSharpSet s ->
        s.Accept {
            new IFSharpSetVisitor<'T -> string> with
                member __.Visit<'a when 'a : comparison> () =
                    let tp = mkPrinter<'a>()
                    wrap(fun (s:Set<'a>) -> s |> Seq.map tp |> String.concat "; " |> sprintf "set [%s]")
        }

    | _ -> failwithf "unsupported type '%O'" typeof<'T>

let p = mkPrinter<(int list * string option) * (bool * unit)> ()
p (([1 .. 5], None), (false, ())) // "(([1; 2; 3; 4; 5], None), (false, ()))"
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

TypeShape can be used to define generic programs that target arbitrary types:
F# records, unions or POCOs. This is achieved using the `IShapeMember` abstraction:
```fsharp
type IShapeMember<'DeclaringType, 'Field> =
    inherit IShapeMember<'DeclaringType>
    abstract Project : 'DeclaringType -> 'Field
    abstract Inject : 'DeclaringType -> 'Field -> 'DeclaringType
```
An F# record then is just a list of member shapes, a union is a list of lists of member shapes.
Member shapes can optionally be configured to generate code at runtime for more performant `Project` and `Inject` operations.
Member shapes come with quoted versions of the API for staged generic programming applications.

To make our pretty printer support these types, we first provide a pretty printer for members:
```fsharp
let mkMemberPrinter (shape : IShapeMember<'DeclaringType>) =
   shape.Accept { new IShapeMemberVisitor<'DeclaringType, 'DeclaringType -> string> with
       member __.Visit (shape : ShapeMember<'DeclaringType, 'Field>) =
           let fieldPrinter = mkPrinter<'Field>()
           fieldPrinter << shape.Project }
```
The for F# records:
```fsharp
    | Shape.FSharpRecord s ->
        s.Accept { new IFSharpRecordVisitor<'T -> string> with
            member __.Visit (s : ShapeFSharpRecord<'Record>) =
                let fieldPrinters : ('Record -> string) [] = s.Fields |> Array.map mkMemberPrinter
                let labels : string [] = s.Fields |> Array.map (fun f -> f.Label)
                wrap(fun (r:'Record) -> sprintf "{ %s = %s }" labels.[0] (fieldPrinters.[0] r)) }
```
Similarly, we could also add support for arbitrary F# unions:
```fsharp
    | Shape.FSharpUnion s ->
        s.Accept { new IFSharpUnionVisitor<'T -> string> with
            member __.Visit (s : ShapeFSharpUnion<'Union>) =
                let cases : ShapeFSharpUnionCase<'Union> [] = s.UnionCases // all union cases
                let mkUnionCasePrinter (case : ShapeFSharpUnionCase<'Union>) =
                    let fieldPrinters = case.Fields |> Array.map mkMemberPrinter
                    (fun (u:'Union) -> 
                        let csvs = fieldPrinters |> Seq.map (fun f -> f r) |> String.concat ", "
                        sprintf "%s(%s)" case.CaseInfo.Name csvs)

                let casePrinters = cases |> Array.map mkUnionCasePrinter // generate printers for all union cases
                wrap(fun (u:'Union) ->
                    let tag : int = s.GetTag u // get the underlying tag for the union case
                    casePrinters.[tag] u) }
```
Similar active patterns exist for classes with settable properties and general POCOs.

### Extensibility

TypeShape can be extended to incorporate new active patterns supporting arbitrary shapes.
Here's an [example](https://github.com/eiriktsarpalis/TypeShape/blob/5dabaf0577d8387c5213a496099598bbd89650b8/src/TypeShape/ISerializableExtensions.fs) 
illustrating how TypeShape can be extended to support ISerializable shapes.

### Additional examples

See the project [samples](https://github.com/eiriktsarpalis/TypeShape/tree/master/samples) folder for more implementations using TypeShape:

* [Printer.fsx](https://github.com/eiriktsarpalis/TypeShape/blob/master/samples/printer.fsx) Pretty printer generator for common F# types.
* [Parser.fsx](https://github.com/eiriktsarpalis/TypeShape/blob/master/samples/parser.fsx) Parser generator for common F# types using FParsec.
* [Equality-Comparer.fsx](https://github.com/eiriktsarpalis/TypeShape/blob/master/samples/equality-comparer.fsx) Equality comparer generator for common F# types.
* [Gmap.fsx](https://github.com/eiriktsarpalis/TypeShape/blob/master/samples/gmap.fsx) A gmap implementation.

### Projects using TypeShape

* [FsPickler](https://github.com/mbraceproject/FsPickler/blob/7d86cbd20ff37899ef58d5430f74376e119b7065/src/FsPickler/PicklerGeneration/PicklerGenerator.fs#L38)
* [FSharp.AWS.DynamoDB](https://github.com/fsprojects/FSharp.AWS.DynamoDB/blob/b5cde91fae2630562188bdff6e16cda0208c330b/src/FSharp.AWS.DynamoDB/Picklers/PicklerResolver.fs#L23)

### Build Status

Head (branch `master`), Build & Unit tests

* Windows/.NET [![Build status](https://ci.appveyor.com/api/projects/status/6t6vovc2xrj8nqh9?svg=true)](https://ci.appveyor.com/project/nessos/typeshape)
* Mac OS X/Mono 4.2 [![Build Status](https://api.travis-ci.org/eiriktsarpalis/TypeShape.svg?branch=master)](https://travis-ci.org/eiriktsarpalis/TypeShape/branches)
