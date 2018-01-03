module TypeShape.Tests.GenericTests

open System
open FSharp.Reflection
open FsCheck
open TypeShape.Core

// Type algebra defining the universe of testable types

type TypeAlg =
    | Primitive of Primitive
    | GroundType of GroundType
    | Option of TypeAlg
    | Ref of TypeAlg
    | List of TypeAlg
    | Array of TypeAlg
    | Map of TypeAlg
    | Set of TypeAlg
    | BinTree of TypeAlg
    | Tuple of NonEmptyArray<TypeAlg>
    | Record of NonEmptyArray<TypeAlg>
    | Union of NonEmptyArray<TypeAlg>

and Primitive =
    | Bool
    | Byte
    | SByte
    | Char
    | Int16
    | Int32
    | Int64
    | UInt16
    | UInt32
    | UInt64
    | Single
    | Double
    //| IntPtr
    //| UIntPtr

and GroundType =
    | Unit
    | String
    | Decimal
    | BigInt
    | DateTime
    | TimeSpan
    | IntEnum
    | CharEnum
    | ByteEnum
    | Peano

module Implementation =

    // Core type implementations

    type Peano = Zero | Succ of Peano

    type BinTree<'T> = Leaf | Node of 'T * left:BinTree<'T> * right:BinTree<'T>

    type IntEnum = A = 1 | B = 2 | C = 3
    type CharEnum = A = 'a' | B = 'b' | C = 'c'
    type ByteEnum = A = 1uy | B = 2uy | C = 4uy

    // Anonymous record implementation

    type Record<'F1> = 
        { Field1 : 'F1 }

    type Record<'F1, 'F2> = 
        { Field1 : 'F1 ; Field2 : 'F2 }

    type Record<'F1, 'F2, 'F3> = 
        { Field1 : 'F1 ; Field2 : 'F2 ; Field3 : 'F3 }

    type Record<'F1, 'F2, 'F3, 'F4> = 
        { Field1 : 'F1 ; Field2 : 'F2 ; Field3 : 'F3 ; Field4 : 'F4 }

    type Record<'F1, 'F2, 'F3, 'F4, 'F5> =
        { Field1 : 'F1 ; Field2 : 'F2 ; Field3 : 'F3 ; Field4 : 'F4
          Field5 : 'F5 }

    type Record<'F1, 'F2, 'F3, 'F4, 'F5, 'F6> =
        { Field1 : 'F1 ; Field2 : 'F2 ; Field3 : 'F3 ; Field4 : 'F4
          Field5 : 'F5 ; Field6 : 'F6 }

    type Record<'F1, 'F2, 'F3, 'F4, 'F5, 'F6, 'F7> =
        { Field1 : 'F1 ; Field2 : 'F2 ; Field3 : 'F3 ; Field4 : 'F4
          Field5 : 'F5 ; Field6 : 'F6 ; Field7 : 'F7 }

    // Anonymous union implementation

    /// Single-case anonymous union
    type Singular<'T> = Singular of 'T

    type FSharpType with
        static member MakeUnionType (args : Type[]) =
            match args.Length with
            | 0 -> invalidArg "args" "must be non-empty array"
            | 1 -> typedefof<Singular<_>>.MakeGenericType args
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

        static member MakeRecordType (args : Type[]) =
            match args.Length with
            | 0 -> invalidArg "args" "must be non-empty array"
            | 1 -> typedefof<Record<_>>.MakeGenericType args
            | 2 -> typedefof<Record<_,_>>.MakeGenericType args
            | 3 -> typedefof<Record<_,_,_>>.MakeGenericType args
            | 4 -> typedefof<Record<_,_,_,_>>.MakeGenericType args
            | 5 -> typedefof<Record<_,_,_,_,_>>.MakeGenericType args
            | 6 -> typedefof<Record<_,_,_,_,_,_>>.MakeGenericType args
            | 7 -> typedefof<Record<_,_,_,_,_,_,_>>.MakeGenericType args
            | _ ->
                let first, rest = args.[..5], args.[5..]
                let restRecord = FSharpType.MakeRecordType rest
                FSharpType.MakeRecordType (Array.append first [|restRecord|])


    // FsCheck Generators
    type NoNaNFloats private () =
        static member Single = 
            Arb.Default.Float32()
            |> Arb.filter (not << Single.IsNaN)

        static member Double =
            Arb.Default.Float()
            |> Arb.filter (not << Double.IsNaN)

    module PrettyPrint =

        let rec typeAlg tAlg =
            match tAlg with
            | Primitive t -> primitive t
            | GroundType t -> groundtype t
            | Option t -> sprintf "%s option" (typeAlg t)
            | Ref t -> sprintf "%s ref" (typeAlg t)
            | List t -> sprintf "%s list" (typeAlg t)
            | Array t -> sprintf "%s []" (typeAlg t)
            | Map t -> sprintf "Map<string, %s>" (typeAlg t)
            | Set t -> sprintf "Set<%s>" (typeAlg t)
            | Tuple (NonEmptyArray [|t|]) -> sprintf "Tuple<%s>" (typeAlg t)
            | Tuple (NonEmptyArray ts) -> 
                ts |> Seq.map typeAlg |> String.concat " * " |> sprintf "(%s)"
            | Union (NonEmptyArray ts) -> 
                ts |> Seq.map typeAlg |> String.concat " + " |> sprintf "(%s)"
            | Record (NonEmptyArray ts) -> 
                ts 
                |> Seq.mapi (fun i t -> sprintf "Field%d : %s" i (typeAlg t)) 
                |> String.concat " ; " 
                |> sprintf "{ %s }"

            | BinTree t -> typeAlg t |> sprintf "BinTree<%s>"

        and primitive prim =
            match prim with
            | Bool -> "bool"
            | Byte -> "byte"
            | SByte -> "sbyte"
            | Char -> "char"
            | Int16 -> "int16"
            | Int32 -> "int32"
            | Int64 -> "int64"
            | UInt16 -> "uint16"
            | UInt32 -> "uint32"
            | UInt64 -> "uint64"
            | Single -> "single"
            | Double -> "double"

        and groundtype gt =
            match gt with
            | Unit -> "unit"
            | String -> "string"
            | Decimal -> "decimal"
            | BigInt -> "bigint"
            | DateTime -> "DateTime"
            | TimeSpan -> "TimeSpan"
            | IntEnum -> "IntEnum"
            | ByteEnum -> "ByteEnum"
            | CharEnum -> "CharEnum"
            | Peano -> "Peano"

    module Mapper =

        let rec typeAlg tAlg =
            match tAlg with
            | Primitive t -> primitive t
            | GroundType t -> groundtype t
            | Option ta -> typedefof<_ option>.MakeGenericType [|typeAlg ta|]
            | Ref ta -> typedefof<_ ref>.MakeGenericType [|typeAlg ta|]
            | List ta -> typedefof<_ list>.MakeGenericType [|typeAlg ta|]
            | Array ta -> (typeAlg ta).MakeArrayType()
            | Map ta -> typedefof<Map<_,_>>.MakeGenericType [|typeof<string> ; typeAlg ta|]
            | Set ta -> typedefof<Set<_>>.MakeGenericType [|typeAlg ta|]
            | BinTree ta -> typedefof<BinTree<_>>.MakeGenericType [|typeAlg ta|]
            | Tuple (NonEmptyArray ts) -> ts |> Array.map typeAlg |> FSharpType.MakeTupleType
            | Union (NonEmptyArray ts) -> ts |> Array.map typeAlg |> FSharpType.MakeUnionType
            | Record (NonEmptyArray ts) -> ts |> Array.map typeAlg |> FSharpType.MakeRecordType

        and primitive t =
            match t with
            | Bool -> typeof<bool>
            | Byte -> typeof<byte>
            | SByte -> typeof<sbyte>
            | Char -> typeof<char>
            | Int16 -> typeof<int16>
            | Int32 -> typeof<int32>
            | Int64 -> typeof<int64>
            | UInt16 -> typeof<uint16>
            | UInt32 -> typeof<uint32>
            | UInt64 -> typeof<uint64>
            | Single -> typeof<single>
            | Double -> typeof<double>

        and groundtype t =
            match t with
            | Unit -> typeof<unit>
            | String -> typeof<string>
            | Decimal -> typeof<decimal>
            | BigInt -> typeof<bigint>
            | DateTime -> typeof<DateTime>
            | TimeSpan -> typeof<TimeSpan>
            | ByteEnum -> typeof<ByteEnum>
            | CharEnum -> typeof<CharEnum>
            | IntEnum -> typeof<IntEnum>
            | Peano -> typeof<Peano>


open Implementation


//------------------------------------------------------------------------------
// Runner Implementation

type IChecker =
    abstract Invoke<'T when 'T : comparison> : TypeAlg -> bool

type IPredicate =
    abstract Invoke<'T when 'T : comparison> : 'T -> bool

type Check with
    /// <summary>
    ///    Generic type checker that runs the checker interface through randomly generated types.
    /// </summary>
    /// <param name="checker">Generic checker interface</param>
    /// <param name="config">FsCheck configuration for type-level random generation</param>
    static member Generic (checker : IChecker, ?config : Config) =
        let config = defaultArg config Config.QuickThrowOnFailure
        let runOnType (tAlg : TypeAlg) =
            let sysType = Mapper.typeAlg tAlg
            match TypeShape.Create sysType with
            | Shape.Comparison s ->
                s.Accept {
                    new IComparisonVisitor<bool> with
                        member __.Visit<'T when 'T : comparison>() =
                            checker.Invoke<'T> tAlg }

            | s -> failwithf "internal error: type %O does not support comparison" s.Type
    
        Check.One(config, runOnType)


    static member GenericPredicate verbose useNaN maxTypes maxTestsPerType (predicate : IPredicate) =
        let tconf = { Config.QuickThrowOnFailure with MaxTest = maxTypes }
        let vconf =
            { Config.QuickThrowOnFailure with 
                QuietOnSuccess = true ;
                MaxTest = maxTestsPerType ; 
                Arbitrary = if useNaN then [] else [typeof<NoNaNFloats>] }

        let checker =
            { new IChecker with
                member __.Invoke<'T when 'T : comparison> tAlg =
                    if verbose then printfn "Testing type %s" (PrettyPrint.typeAlg tAlg)
                    Check.One(vconf, fun (t:'T) -> predicate.Invoke t) ; true }

        Check.Generic(checker, tconf)