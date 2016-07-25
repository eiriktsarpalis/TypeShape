# TypeShape

TypeShape is a small F# library for practical generic programming.
Borrows from ideas used in the FsPickler [implementation](http://mbraceproject.github.io/FsPickler/overview.html#Pickler-Generation).
Uses a combination of active patterns and F# object expressions and minimizes the
amount of reflection and unsafe code required by the user in such applications.

TypeShape is not a metaprogramming library and does not emit IL at runtime;
It only relies on basic reflection APIs.

### Installing

To incorporate TypeShape in your project place the following line in your
`paket.dependencies` file:
```
github eiriktsarpalis/TypeShape src/TypeShape/TypeShape.fs
```
and in `paket.refences`:
```
File: TypeShape.fs
```
You can hide the TypeShape API from your project 
by enabling the `TYPESHAPE_HIDE` build conditional.

### Example: Implementing a value printer

```fsharp
open System
open TypeShape

let rec mkPrinter<'T> () : 'T -> string = mkPrinterUntyped typeof<'T> :?> _
and private mkPrinterUntyped (t : Type) : obj =
    match getShape t with
    | Shape.Unit -> box(fun () -> "()")
    | Shape.Bool -> box(sprintf "%b")
    | Shape.Int32 -> box(sprintf "%d")
    | Shape.String -> box(sprintf "\"%s\"")
    | Shape.FSharpOption s ->
        s.Accept {
            new IFSharpOptionVisitor<obj> with
                member __.Visit<'T> () =
                    let tp = mkPrinter<'T>()
                    box(function None -> "None" | Some t -> sprintf "Some (%s)" (tp t))
        }

    | Shape.FSharpList s ->
        s.Accept {
            new IFSharpListVisitor<obj> with
                member __.Visit<'T> () =
                    let tp = mkPrinter<'T>()
                    box(fun ts -> ts |> List.map tp |> String.concat "; " |> sprintf "[%s]")
        }

    | Shape.Tuple2 s ->
        s.Accept {
            new ITuple2Visitor<obj> with
                member __.Visit<'T, 'S> () =
                    let tp = mkPrinter<'T>()
                    let sp = mkPrinter<'S>()
                    box(fun (t : 'T, s : 'S) -> sprintf "(%s, %s)" (tp t) (sp s))
        }

    | _ -> failwithf "unsupported type '%O'" t

let p = mkPrinter<(int list * string option) * (bool * unit)> ()
p (([1 .. 5], None), (false, ())) // "(([1; 2; 3; 4; 5], None), (false, ()))"
```
Let's see how the value printer compares to sprintf:
```fsharp
#time "on"
let value = (([1 .. 5], Some "42"), (false, ()))

// Real: 00:00:00.561, CPU: 00:00:00.562, GC gen0: 32, gen1: 0, gen2: 0
for i = 1 to 1000 do ignore <| sprintf "%A" value
// Real: 00:00:00.010, CPU: 00:00:00.000, GC gen0: 1, gen1: 0, gen2: 0
for i = 1 to 1000 do ignore <| p value
```

### Build Status

Head (branch `master`), Build & Unit tests

* Windows/.NET [![Build status](https://ci.appveyor.com/api/projects/status/6t6vovc2xrj8nqh9?svg=true)](https://ci.appveyor.com/project/nessos/typeshape)
* Mac OS X/Mono 4.2 [![Build Status](https://travis-ci.org/eiriktsarpalis/TypeShape.png?branch=master)](https://travis-ci.org/eiriktsarpalis/TypeShape/branches)
