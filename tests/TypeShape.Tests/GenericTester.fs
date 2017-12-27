module TypeShape.Tests.GenericTests

open System
open FSharp.Reflection
open FsCheck
open TypeShape.Core

// Type algebra defining the universe of testable types

type TypeAlg =
    | Unit
    | Primitive of Primitive
    | Option of TypeAlg
    | Ref of TypeAlg
    | List of TypeAlg
    | Array of TypeAlg
    | Tuple of TypeAlg []
    | Record of TypeAlg []
    | Union of TypeAlg []

and Primitive =
    | Bool
    | Byte
    | Int16
    | Int32
    | Int64
    | UInt16
    | UInt32
    | UInt64
    | Single
    | Double
    | Decimal
    | BigInt
    | DateTime
    | TimeSpan

module Implementation =

    // Anonymous record implementation

    // Nullary record representation
    type Record = { Null : unit }

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

    // While strictly speaking not a nullary union (which is the bottom type),
    // we use this as a convenient representation
    type Nil = Nil

    /// Single-case anonymous union
    type Singular<'T> = Singular of 'T

    type FSharpType with
        static member MakeUnionType (args : Type[]) =
            match args.Length with
            | 0 -> typeof<Nil>
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
            | 0 -> typeof<Record>
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


        static member MakeTupleType2 (args : Type[]) =
            match args.Length with
            | 0 -> typeof<unit>
            | _ -> FSharpType.MakeTupleType args


type Primitive with
    member p.Name =
        match p with
        | Bool -> "bool"
        | Byte -> "byte"
        | Int16 -> "int16"
        | Int32 -> "int32"
        | Int64 -> "int64"
        | UInt16 -> "uint16"
        | UInt32 -> "uint32"
        | UInt64 -> "uint64"
        | Single -> "single"
        | Double -> "double"
        | Decimal -> "decimal"
        | BigInt -> "bigint"
        | DateTime -> "DateTime"
        | TimeSpan -> "TimeSpan"

    member p.Type =
        match p with
        | Bool -> typeof<bool>
        | Byte -> typeof<byte>
        | Int16 -> typeof<int16>
        | Int32 -> typeof<int32>
        | Int64 -> typeof<int64>
        | UInt16 -> typeof<uint16>
        | UInt32 -> typeof<uint32>
        | UInt64 -> typeof<uint64>
        | Single -> typeof<single>
        | Double -> typeof<double>
        | Decimal -> typeof<decimal>
        | BigInt -> typeof<bigint>
        | DateTime -> typeof<DateTime>
        | TimeSpan -> typeof<TimeSpan>

open Implementation

type TypeAlg with
    member t.Name =
        let rec aux tAlg =
            match tAlg with
            | Unit -> "unit"
            | Primitive p -> p.Name
            | Option t -> sprintf "%s option" (aux t)
            | Ref t -> sprintf "%s ref" (aux t)
            | List t -> sprintf "%s list" (aux t)
            | Array t -> sprintf "%s []" (aux t)
            | Tuple [||] -> "unit"
            | Tuple [|t|] -> aux t
            | Tuple ts -> ts |> Seq.map aux |> String.concat " * " |> sprintf "(%s)"
            | Union [||] -> "unit"
            | Union [|t|] -> aux t
            | Union ts -> ts |> Seq.map aux |> String.concat " + " |> sprintf "(%s)"
            | Record [||] -> "unit"
            | Record ts -> 
                ts 
                |> Seq.mapi (fun i t -> sprintf "Field%d : %s" i (aux t)) 
                |> String.concat " ; " 
                |> sprintf "{ %s }"

        aux t

    /// System.Type mapping of a Type Algebra instance
    member t.Type =
        let rec aux tAlg =
            match tAlg with
            | Unit -> typeof<unit>
            | Primitive p -> p.Type
            | Option ta -> typedefof<_ option>.MakeGenericType [|aux ta|]
            | Ref ta -> typedefof<_ ref>.MakeGenericType [|aux ta|]
            | List ta -> typedefof<_ list>.MakeGenericType [|aux ta|]
            | Array ta -> (aux ta).MakeArrayType()
            | Tuple ts -> ts |> Array.map aux |> FSharpType.MakeTupleType2
            | Union ts -> ts |> Array.map aux |> FSharpType.MakeUnionType
            | Record ts -> ts |> Array.map aux |> FSharpType.MakeRecordType

        aux t


//------------------------------------------------------------------------------
// Runner Implementation

type IChecker =
    abstract Invoke<'T when 'T : comparison> : unit -> bool

type IPredicate =
    abstract Invoke<'T when 'T : comparison> : 'T -> bool

type NoNaNFloats private () =
    static member Single = 
        Arb.Default.Float32()
        |> Arb.filter (not << Single.IsNaN)

    static member Double =
        Arb.Default.Float()
        |> Arb.filter (not << Double.IsNaN)

type Check with
    /// <summary>
    ///    Generic type checker that runs the checker interface through randomly generated types.
    /// </summary>
    /// <param name="checker">Generic checker interface</param>
    /// <param name="config">FsCheck configuration for type-level random generation</param>
    static member Generic (checker : IChecker, ?config : Config) =
        let config = defaultArg config Config.QuickThrowOnFailure
        let runOnType (tAlg : TypeAlg) =
            match TypeShape.Create tAlg.Type with
            | Shape.Comparison s ->
                s.Accept {
                    new IComparisonVisitor<bool> with
                        member __.Visit<'T when 'T : comparison>() =
                            printfn "Testing type: %s" tAlg.Name
                            checker.Invoke<'T> () }

            | s -> failwithf "internal error: type %O does not support comparison" s.Type
    
        Check.One(config, runOnType)


    static member GenericPredicate useNaN maxTypes maxTestsPerType (predicate : IPredicate) =
        let tconf = { Config.QuickThrowOnFailure with MaxTest = maxTypes }
        let vconf =
            { Config.QuickThrowOnFailure with 
                QuietOnSuccess = true ;
                MaxTest = maxTestsPerType ; 
                Arbitrary = if useNaN then [] else [typeof<NoNaNFloats>] }

        let checker =
            { new IChecker with
                member __.Invoke<'T when 'T : comparison> () =
                    Check.One(vconf, fun (t:'T) -> predicate.Invoke t) ; true }

        Check.Generic(checker, tconf)