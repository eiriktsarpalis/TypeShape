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
    | Tuple of isStruct:bool * elems:NonEmptyArray<TypeAlg>
    | Record of isStruct:bool * fields:NonEmptyArray<TypeAlg>
    | Union of isStruct:bool * fields:NonEmptyArray<TypeAlg>

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
    | IntNullable
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

    // Anonymous struct record implementation

    [<Struct>]
    type SRecord<'F1> = 
        { Field1 : 'F1 }

    [<Struct>]
    type SRecord<'F1, 'F2> = 
        { Field1 : 'F1 ; Field2 : 'F2 }

    [<Struct>]
    type SRecord<'F1, 'F2, 'F3> = 
        { Field1 : 'F1 ; Field2 : 'F2 ; Field3 : 'F3 }

    [<Struct>]
    type SRecord<'F1, 'F2, 'F3, 'F4> = 
        { Field1 : 'F1 ; Field2 : 'F2 ; Field3 : 'F3 ; Field4 : 'F4 }

    [<Struct>]
    type SRecord<'F1, 'F2, 'F3, 'F4, 'F5> =
        { Field1 : 'F1 ; Field2 : 'F2 ; Field3 : 'F3 ; Field4 : 'F4
          Field5 : 'F5 }

    [<Struct>]
    type SRecord<'F1, 'F2, 'F3, 'F4, 'F5, 'F6> =
        { Field1 : 'F1 ; Field2 : 'F2 ; Field3 : 'F3 ; Field4 : 'F4
          Field5 : 'F5 ; Field6 : 'F6 }

    [<Struct>]
    type SRecord<'F1, 'F2, 'F3, 'F4, 'F5, 'F6, 'F7> =
        { Field1 : 'F1 ; Field2 : 'F2 ; Field3 : 'F3 ; Field4 : 'F4
          Field5 : 'F5 ; Field6 : 'F6 ; Field7 : 'F7 }

    // Anonymous union implementation

    /// Single-case anonymous union
    type Choice<'T> = Choice1Of1 of 'T

    [<Struct>] type SChoice<'F1> = Choice1Of1 of f1:'F1
    [<Struct>] type SChoice<'F1, 'F2> = Choice1Of2 of f1:'F1 | Choice2Of2 of f2:'F2
    [<Struct>] type SChoice<'F1, 'F2, 'F3> = Choice1Of3 of f1:'F1 | Choice2Of3 of f2:'F2 | Choice3Of3 of f3:'F3
    [<Struct>] type SChoice<'F1, 'F2, 'F3, 'F4> = Choice1Of4 of f1:'F1 | Choice2Of4 of f2:'F2 | Choice3Of4 of f3:'F3 | Choice4Of4 of f4:'F4
    [<Struct>] type SChoice<'F1, 'F2, 'F3, 'F4, 'F5> = Choice1Of5 of f1:'F1 | Choice2Of5 of f2:'F2 | Choice3Of5 of f3:'F3 | Choice4Of5 of f4:'F4 | Choice5Of5 of f5:'F5
    [<Struct>] type SChoice<'F1, 'F2, 'F3, 'F4, 'F5, 'F6> = Choice1Of6 of f1:'F1 | Choice2Of6 of f2:'F2 | Choice3Of6 of f3:'F3 | Choice4Of6 of f4:'F4 | Choice5Of6 of f5:'F5 | Choice6Of6 of f6:'F6
    [<Struct>] type SChoice<'F1, 'F2, 'F3, 'F4, 'F5, 'F6, 'F7> = Choice1Of7 of f1:'F1 | Choice2Of7 of f2:'F2 | Choice3Of7 of f3:'F3 | Choice4Of7 of f4:'F4 | Choice5Of7 of f5:'F5 | Choice6Of7 of f6:'F6 | Choice7Of7 of f7:'F7

    type FSharpType with
        static member MakeUnionType isStruct (args : Type[]) =
            match isStruct, args.Length with
            | false, 0 -> invalidArg "args" "must be non-empty array"
            | false, 1 -> typedefof<Choice<_>>.MakeGenericType args
            | false, 2 -> typedefof<Choice<_,_>>.MakeGenericType args
            | false, 3 -> typedefof<Choice<_,_,_>>.MakeGenericType args
            | false, 4 -> typedefof<Choice<_,_,_,_>>.MakeGenericType args
            | false, 5 -> typedefof<Choice<_,_,_,_,_>>.MakeGenericType args
            | false, 6 -> typedefof<Choice<_,_,_,_,_,_>>.MakeGenericType args
            | false, 7 -> typedefof<Choice<_,_,_,_,_,_,_>>.MakeGenericType args
            | false, _ ->
                let first, rest = args.[..5], args.[5..]
                let restUnion = FSharpType.MakeUnionType false rest
                FSharpType.MakeUnionType false (Array.append first [|restUnion|])

            | true, 0 -> invalidArg "args" "must be non-empty array"
            | true, 1 -> typedefof<SChoice<_>>.MakeGenericType args
            | true, 2 -> typedefof<SChoice<_,_>>.MakeGenericType args
            | true, 3 -> typedefof<SChoice<_,_,_>>.MakeGenericType args
            | true, 4 -> typedefof<SChoice<_,_,_,_>>.MakeGenericType args
            | true, 5 -> typedefof<SChoice<_,_,_,_,_>>.MakeGenericType args
            | true, 6 -> typedefof<SChoice<_,_,_,_,_,_>>.MakeGenericType args
            | true, 7 -> typedefof<SChoice<_,_,_,_,_,_,_>>.MakeGenericType args
            | true, _ ->
                let first, rest = args.[..5], args.[5..]
                let restUnion = FSharpType.MakeUnionType false rest
                FSharpType.MakeUnionType false (Array.append first [|restUnion|])

        static member MakeRecordType isStruct (args : Type[]) =
            match isStruct, args.Length with
            | true, 0 -> invalidArg "args" "must be non-empty array"
            | true, 1 -> typedefof<Record<_>>.MakeGenericType args
            | true, 2 -> typedefof<Record<_,_>>.MakeGenericType args
            | true, 3 -> typedefof<Record<_,_,_>>.MakeGenericType args
            | true, 4 -> typedefof<Record<_,_,_,_>>.MakeGenericType args
            | true, 5 -> typedefof<Record<_,_,_,_,_>>.MakeGenericType args
            | true, 6 -> typedefof<Record<_,_,_,_,_,_>>.MakeGenericType args
            | true, 7 -> typedefof<Record<_,_,_,_,_,_,_>>.MakeGenericType args
            | true, _ ->
                let first, rest = args.[..5], args.[5..]
                let restRecord = FSharpType.MakeRecordType true rest
                FSharpType.MakeRecordType true (Array.append first [|restRecord|])

            | false, 0 -> invalidArg "args" "must be non-empty array"
            | false, 1 -> typedefof<SRecord<_>>.MakeGenericType args
            | false, 2 -> typedefof<SRecord<_,_>>.MakeGenericType args
            | false, 3 -> typedefof<SRecord<_,_,_>>.MakeGenericType args
            | false, 4 -> typedefof<SRecord<_,_,_,_>>.MakeGenericType args
            | false, 5 -> typedefof<SRecord<_,_,_,_,_>>.MakeGenericType args
            | false, 6 -> typedefof<SRecord<_,_,_,_,_,_>>.MakeGenericType args
            | false, 7 -> typedefof<SRecord<_,_,_,_,_,_,_>>.MakeGenericType args
            | false, _ ->
                let first, rest = args.[..5], args.[5..]
                let restRecord = FSharpType.MakeRecordType false rest
                FSharpType.MakeRecordType false (Array.append first [|restRecord|])


    // FsCheck Generators

    type NoNaNFloats =
        static member Single =
            Arb.Default.Float32()
            |> Arb.filter Single.IsFinite

        static member Double =
            Arb.Default.Float()
            |> Arb.filter Double.IsFinite


    type Config with
        static member CreateTypeConfig(maxTypes : int, ?maxSize : int) =
            { Config.QuickThrowOnFailure with 
                MaxTest = maxTypes ; 
                EndSize = defaultArg maxSize 20 }

        static member CreateValueConfig(useNaN : bool, maxTests : int, ?arbitrary : Type) =
            { Config.QuickThrowOnFailure with 
                QuietOnSuccess = true ;
                MaxTest = maxTests ; 
                Arbitrary = 
                    [ if not useNaN then typeof<NoNaNFloats> 
                      match arbitrary with Some a -> a | None -> () ] }

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
            | Tuple (isStruct, NonEmptyArray ts) ->
                ts 
                |> Seq.map typeAlg 
                |> String.concat " * " 
                |> if isStruct then sprintf "struct (%s)" else sprintf "(%s)"

            | Union (isStruct, NonEmptyArray ts) -> 
                ts 
                |> Seq.map typeAlg 
                |> String.concat " + " 
                |> if isStruct then sprintf "struct (%s)" else sprintf "(%s)"

            | Record (isStruct, NonEmptyArray ts) -> 
                ts 
                |> Seq.mapi (fun i t -> sprintf "Field%d : %s" i (typeAlg t)) 
                |> String.concat " ; " 
                |> if isStruct then sprintf "struct { %s }" else sprintf "{ %s }"

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
            | IntNullable -> "Nullable<int>"
            | ByteEnum -> "ByteEnum"
            | CharEnum -> "CharEnum"
            | Peano -> "Peano"

    module Mapper =

        let private structTupleAssembly = typeof<struct(int * int)>.Assembly

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
            | Tuple (isStruct = false; elems = NonEmptyArray ts) -> ts |> Array.map typeAlg |> FSharpType.MakeTupleType
            | Tuple (isStruct = true ; elems = NonEmptyArray ts) -> ts |> Array.map typeAlg |> FSharpType.MakeTupleType
            | Union (isStruct, NonEmptyArray ts) -> ts |> Array.map typeAlg |> FSharpType.MakeUnionType isStruct
            | Record (isStruct, NonEmptyArray ts) -> ts |> Array.map typeAlg |> FSharpType.MakeRecordType isStruct

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
            | IntNullable -> typeof<Nullable<int>>
            | Peano -> typeof<Peano>


    type TypeAlg with
        member tAlg.TypeShape : TypeShape =
            let sysType = Mapper.typeAlg tAlg
            TypeShape.Create sysType

        member tAlg.Accept (v:ITypeVisitor<'R>) : 'R =
            tAlg.TypeShape.Accept v

        member tAlg.Accept (v:IEqualityVisitor<'R>) : 'R =
            match tAlg.TypeShape with
            | Shape.Equality s -> s.Accept v
            | s -> failwithf "internal error: type %O does not support equality" s.Type


open Implementation


//------------------------------------------------------------------------------
// Runner Implementation

type Checker =
    abstract Invoke<'T when 'T : equality> : TypeAlg -> bool

type Predicate =
    abstract Invoke<'T when 'T : equality> : 'T -> bool

type Predicate2 =
    abstract Invoke<'T when 'T : equality> : 'T -> 'T -> bool

type Predicate3 =
    abstract Invoke<'T when 'T : equality> : 'T -> 'T -> 'T -> bool

type Check with
    /// <summary>
    ///    Generic type checker that runs the checker interface through randomly generated types.
    /// </summary>
    /// <param name="checker">Generic checker interface.</param>
    /// <param name="config">FsCheck configuration for type-level random generation.</param>
    /// <param name="verbose">Output every random type that is being tested. Defaults to false.</param>
    static member Generic (checker : Checker, ?config : Config, ?verbose : bool) =
        let config = defaultArg config Config.QuickThrowOnFailure
        let verbose = defaultArg verbose false
        let runOnType (tAlg : TypeAlg) =
            tAlg.Accept {  new IEqualityVisitor<bool> with
                member _.Visit<'T when 'T : equality>() =
                    if verbose then printfn "Testing type %s" (PrettyPrint.typeAlg tAlg)
                    checker.Invoke<'T> tAlg }
    
        Check.One<TypeAlg -> bool>(config, runOnType)

    /// <summary>
    ///     Runs a property test given provided generic predicate.
    /// </summary>
    /// <param name="verbose">Print tested type information to console.</param>
    /// <param name="arbitrary">Additional arbitrary source for value generation.</param>
    /// <param name="useNaN">Generate NaNs when the random information contains floats.</param>
    /// <param name="maxTypes">Maximum number of randomly generated types.</param>
    /// <param name="maxTestsPerType">Maximum number of randomly generated values per type.</param>
    /// <param name="predicate">Predicate to check.</param>
    static member GenericPredicate verbose arbitrary useNaN maxTypes maxTestsPerType (predicate : Predicate) =
        let tconf = Config.CreateTypeConfig(maxTypes)
        let vconf = Config.CreateValueConfig(useNaN, maxTestsPerType, ?arbitrary = arbitrary)

        let checker =
            { new Checker with
                member _.Invoke<'T when 'T : equality> tAlg =
                    Check.One<'T -> bool>(vconf, predicate.Invoke) ; true }

        Check.Generic(checker, tconf, verbose = verbose)

    /// <summary>
    ///     Runs a property test given provided generic predicate.
    /// </summary>
    /// <param name="verbose">Print tested type information to console.</param>
    /// <param name="useNaN">Generate NaNs when the random information contains floats.</param>
    /// <param name="maxTypes">Maximum number of randomly generated types.</param>
    /// <param name="maxTestsPerType">Maximum number of randomly generated values per type.</param>
    /// <param name="predicate2">Predicate to check.</param>
    static member GenericPredicate2 verbose useNaN maxTypes maxTestsPerType (predicate2 : Predicate2) =
        let tconf = Config.CreateTypeConfig(maxTypes)
        let vconf = Config.CreateValueConfig(useNaN, maxTestsPerType)

        let checker =
            { new Checker with
                member _.Invoke<'T when 'T : equality> tAlg =
                    Check.One<'T * 'T -> bool>(vconf, fun (t1,t2) -> predicate2.Invoke t1 t2) ; true }

        Check.Generic(checker, tconf, verbose = verbose)

    /// <summary>
    ///     Runs a property test given provided generic predicate.
    /// </summary>
    /// <param name="verbose">Print tested type information to console.</param>
    /// <param name="useNaN">Generate NaNs when the random information contains floats.</param>
    /// <param name="maxTypes">Maximum number of randomly generated types.</param>
    /// <param name="maxTestsPerType">Maximum number of randomly generated values per type.</param>
    /// <param name="predicate3">Predicate to check.</param>
    static member GenericPredicate3 verbose useNaN maxTypes maxTestsPerType (predicate3 : Predicate3) =
        let tconf = Config.CreateTypeConfig(maxTypes)
        let vconf = Config.CreateValueConfig(useNaN, maxTestsPerType)

        let checker =
            { new Checker with
                member _.Invoke<'T when 'T : equality> tAlg =
                    Check.One<'T * 'T * 'T -> bool>(vconf, fun (t1,t2,t3) -> predicate3.Invoke t1 t2 t3) ; true }

        Check.Generic(checker, tconf, verbose = verbose)