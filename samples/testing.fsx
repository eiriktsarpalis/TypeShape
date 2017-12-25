#r "../bin/Release/net40/TypeShape.dll"
#r "../bin/Release/net40/FsCheck.dll"

open System
open FSharp.Reflection
open FsCheck
open TypeShape

// Provides facility for testing generic programs:
// uses FsCheck to auto-generate types from a fixed algebra,
// then uses TypeShape to run nested checks for values of each type.

/// Abstraction for testing generic programs
type IPredicate =
    abstract Invoke<'T> : value:'T -> bool

/// Type Algebra we will be testing our code against
type TypeAlg =
    | Unit
    | Bool
    | Int32
    | Decimal
    | String
    | Option of TypeAlg
    | Ref of TypeAlg
    | List of TypeAlg
    | Array of TypeAlg
    | Tuple of TypeAlg []
    | Union of TypeAlg []

/// Type Algebra pretty-printer
let rec toString (tAlg : TypeAlg) =
    match tAlg with
    | Unit -> "unit"
    | Bool -> "bool"
    | Int32 -> "int"
    | Decimal -> "decimal"
    | String -> "string"
    | Option t -> sprintf "%s option" (toString t)
    | Ref t -> sprintf "%s ref" (toString t)
    | List t -> sprintf "%s list" (toString t)
    | Array t -> sprintf "%s []" (toString t)
    | Tuple [||] -> "unit"
    | Tuple [|t|] -> toString t
    | Tuple ts -> ts |> Seq.map toString |> String.concat " * " |> sprintf "(%s)"
    | Union [||] -> "unit"
    | Union [|t|] -> toString t
    | Union ts -> ts |> Seq.map toString |> String.concat " + " |> sprintf "(%s)"

type FSharpType with
    static member MakeUnionType (args : Type[]) =
        match args.Length with
        | 0 -> typeof<unit> // strictly speaking not a nullary union, but void wouldn't suit us here
        | 1 -> args.[0]
        | 2 -> typedefof<Choice<_,_>>.MakeGenericType args
        | 3 -> typedefof<Choice<_,_,_>>.MakeGenericType args
        | 4 -> typedefof<Choice<_,_,_,_>>.MakeGenericType args
        | 5 -> typedefof<Choice<_,_,_,_,_>>.MakeGenericType args
        | 6 -> typedefof<Choice<_,_,_,_,_,_>>.MakeGenericType args
        | 7 -> typedefof<Choice<_,_,_,_,_,_,_>>.MakeGenericType args
        | _ ->
            let first, rest = args.[..5], args.[5..]
            let restUnion = FSharpType.MakeUnionType rest
            FSharpType.MakeUnionType (Array.append first [|restUnion|])

/// Converts a TypeAlg representation to its corresponding System.Type
let rec toSysType (tAlg : TypeAlg) : Type =
    match tAlg with
    | Unit -> typeof<unit>
    | Bool -> typeof<bool>
    | Int32 -> typeof<int>
    | Decimal -> typeof<decimal>
    | String -> typeof<string>
    | Option ta -> typedefof<_ option>.MakeGenericType [|toSysType ta|]
    | Ref ta -> typedefof<_ ref>.MakeGenericType [|toSysType ta|]
    | List ta -> typedefof<_ list>.MakeGenericType [|toSysType ta|]
    | Array ta -> (toSysType ta).MakeArrayType()
    | Tuple [||] -> typeof<unit>
    | Tuple [|t|] -> toSysType t
    | Tuple ts -> ts |> Array.map toSysType |> FSharpType.MakeTupleType
    | Union ts -> ts |> Array.map toSysType |> FSharpType.MakeUnionType


/// <summary>
///     Maximum number of generated types
/// </summary>
/// <param name="maxTypes">Maximum number of types to generated</param>
/// <param name="maxTestsPerType">Maximum number of values to test per type</param>
/// <param name="predicate">Generic predicate to test</param>
let check (maxTypes : int) (maxTestsPerType : int) (predicate : IPredicate) =
    let tconf = { Config.QuickThrowOnFailure with MaxTest = maxTypes }
    let vconf = { Config.QuickThrowOnFailure with MaxTest = maxTestsPerType }
    let runOnType (tAlg : TypeAlg) =
        let sysType = toSysType tAlg
        let shape = TypeShape.Create sysType
        shape.Accept {
            new ITypeShapeVisitor<bool> with
                member __.Visit<'T>() =
                    printfn "Testing type: %s" (toString tAlg)
                    Check.One(vconf, fun (t:'T) -> predicate.Invoke t)
                    true
        }
    
    Check.One(tconf, runOnType)

// test the setup

#load "equality-comparer.fsx"
open ``Equality-comparer``

// check if our equality comparer satisfies reflexivivity
check 100 100 
    { new IPredicate with 
        member __.Invoke (t : 'T) =
            let cmp = comparer<'T>
            cmp.Equals(t,t) }


check 100 100 
    { new IPredicate with 
        member __.Invoke (t : 'T) =
            let cmp = comparer<'T>
            cmp.Equals(t,t) }