#r "../bin/TypeShape.dll"
#r "../bin/FsCheck.dll"

open System
open FsCheck
open TypeShape

// Provides facility for testing generic programs:
// uses FsCheck to auto-generate types from a fixed algebra,
// then uses TypeShape to run nested checks for values of each type.

/// Abstraction for testing generic programs
type IPredicate =
    abstract Invoke<'T> : value:'T -> bool

/// Type Algebra implementation
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
    | Tuple2 of TypeAlg * TypeAlg
    | Tuple3 of TypeAlg * TypeAlg * TypeAlg

/// Type Algebra pretty-printer
let rec toString (atype : TypeAlg) =
    match atype with
    | Unit -> "unit"
    | Bool -> "bool"
    | Int32 -> "int"
    | Decimal -> "decimal"
    | String -> "string"
    | Option t -> sprintf "%s option" (toString t)
    | Ref t -> sprintf "%s ref" (toString t)
    | List t -> sprintf "%s list" (toString t)
    | Array t -> sprintf "%s []" (toString t)
    | Tuple2 (t1,t2) -> sprintf "(%s * %s)" (toString t1) (toString t2)
    | Tuple3 (t1,t2,t3) -> sprintf "(%s * %s * %s)" (toString t1) (toString t2) (toString t3)

/// Converts a TypeAlg representation to its corresponding System.Type
let rec toSysType (atype : TypeAlg) : Type =
    match atype with
    | Unit -> typeof<unit>
    | Bool -> typeof<bool>
    | Int32 -> typeof<int>
    | Decimal -> typeof<decimal>
    | String -> typeof<string>
    | Option ta -> typedefof<_ option>.MakeGenericType [|toSysType ta|]
    | Ref ta -> typedefof<_ ref>.MakeGenericType [|toSysType ta|]
    | List ta -> typedefof<_ list>.MakeGenericType [|toSysType ta|]
    | Array ta -> (toSysType ta).MakeArrayType()
    | Tuple2 (t1,t2) -> typedefof<_ * _>.MakeGenericType [|toSysType t1; toSysType t2|]
    | Tuple3 (t1,t2,t3) -> typedefof<_ * _ * _>.MakeGenericType [|toSysType t1; toSysType t2; toSysType t3|]


/// <summary>
///     Maximum number of generated types
/// </summary>
/// <param name="maxTypes">Maximum number of types to generated</param>
/// <param name="maxTestsPerType">Maximum number of values to test per type</param>
/// <param name="predicate">Generic predicate to test</param>
let check (maxTypes : int) (maxTestsPerType : int) (predicate : IPredicate) =
    let tconf = { Config.QuickThrowOnFailure with MaxTest = maxTypes }
    let vconf = { Config.QuickThrowOnFailure with MaxTest = maxTestsPerType }
    let runOnType (atype : TypeAlg) =
        let stype = toSysType atype
        let shape = TypeShape.Create stype
        shape.Accept {
            new ITypeShapeVisitor<bool> with
                member __.Visit<'T>() =
                    printfn "Testing type: %s" (toString atype)
                    Check.One(vconf, fun (t:'T) -> predicate.Invoke t)
                    true
        }
    
    Check.One(tconf, runOnType)

// test the setup

#load "equality-comparer.fsx"
open ``Equality-comparer``

// check if our equality comparer satisfies reflexivivity
check 10 100 
    { new IPredicate with 
        member __.Invoke (t : 'T) =
            let cmp = comparer<'T>
            cmp.Equals(t,t) }