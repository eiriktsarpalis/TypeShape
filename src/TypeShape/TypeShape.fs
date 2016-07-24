module TypeShape

#nowarn "4224"

open System
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Reflection

//////////////////////////////////////////////////
///////////// Section: TypeShape core definitions

type ITypeShapeVisitor<'R> =
    abstract Visit<'T> : unit -> 'R

[<AbstractClass>]
type TypeShape internal () =
    abstract Type : Type
    abstract Accept : ITypeShapeVisitor<'R> -> 'R
    override s.ToString() = sprintf "Shape [%O]" (s.GetType())

type TypeShape<'T> =
    inherit TypeShape

    [<CompilerMessage("TypeShape<'T> constructor should only be used when inheriting shape implementations.", 4224)>]
    new () = { inherit TypeShape() }
    override __.Type = typeof<'T>
    override __.Accept v = v.Visit<'T> ()




//////////////////////////////////////
///////////// Section: Core BCL types

///////////// Enum types

type IEnumVisitor<'R> =
    abstract Visit<'Enum, 'Underlying when 'Enum : enum<'Underlying>> : unit -> 'R

type IShapeEnum =
    abstract Accept : IEnumVisitor<'R> -> 'R

type ShapeEnum<'Enum, 'Underlying when 'Enum : enum<'Underlying>>() =
    inherit TypeShape<'Enum> ()
    interface IShapeEnum with
        member __.Accept v = v.Visit<'Enum, 'Underlying> ()

///////////// Nullable types

type INullableVisitor<'R> =
    abstract Visit<'T when 'T : (new : unit -> 'T) and 'T :> ValueType and 'T : struct> : unit -> 'R

type IShapeNullable =
    abstract Accept : INullableVisitor<'R> -> 'R

type ShapeNullable<'T when 'T : (new : unit -> 'T) and 'T :> ValueType and 'T : struct>() =
    inherit TypeShape<Nullable<'T>>()
    interface IShapeNullable with
        member __.Accept v = v.Visit<'T> ()

///////////// System.Tuple

type ITuple1Visitor<'R> =
    abstract Visit<'T> : unit -> 'R

type IShapeTuple1 =
    abstract Accept : ITuple1Visitor<'R> -> 'R

type ShapeTuple<'T> () =
    inherit TypeShape<System.Tuple<'T>> ()
    interface IShapeTuple1 with
        member __.Accept v = v.Visit<'T> ()

type ITuple2Visitor<'R> =
    abstract Visit<'T1, 'T2> : unit -> 'R

type IShapeTuple2 =
    abstract Accept : ITuple2Visitor<'R> -> 'R

type ShapeTuple<'T1, 'T2> () =
    inherit TypeShape<'T1 * 'T2> ()
    interface IShapeTuple2 with
        member __.Accept v = v.Visit<'T1,'T2> ()

type ITuple3Visitor<'R> =
    abstract Visit<'T1, 'T2, 'T3> : unit -> 'R

type IShapeTuple3 =
    abstract Accept : ITuple3Visitor<'R> -> 'R

type ShapeTuple<'T1, 'T2, 'T3> () =
    inherit TypeShape<'T1 * 'T2 * 'T3> ()
    interface IShapeTuple3 with
        member __.Accept v = v.Visit<'T1, 'T2, 'T3> ()

type ITuple4Visitor<'R> =
    abstract Visit<'T1, 'T2, 'T3, 'T4> : unit -> 'R

type IShapeTuple4 =
    abstract Accept : ITuple4Visitor<'R> -> 'R

type ShapeTuple<'T1, 'T2, 'T3, 'T4> () =
    inherit TypeShape<'T1 * 'T2 * 'T3 * 'T4> ()
    interface IShapeTuple4 with
        member __.Accept v = v.Visit<'T1, 'T2, 'T3, 'T4> ()

type ITuple5Visitor<'R> =
    abstract Visit<'T1, 'T2, 'T3, 'T4, 'T5> : unit -> 'R

type IShapeTuple5 =
    abstract Accept : ITuple5Visitor<'R> -> 'R

type ShapeTuple<'T1, 'T2, 'T3, 'T4, 'T5> () =
    inherit TypeShape<'T1 * 'T2 * 'T3 * 'T4 * 'T5> ()
    interface IShapeTuple5 with
        member __.Accept v = v.Visit<'T1, 'T2, 'T3, 'T4, 'T5> ()

type ITuple6Visitor<'R> =
    abstract Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> : unit -> 'R

type IShapeTuple6 =
    abstract Accept : ITuple6Visitor<'R> -> 'R

type ShapeTuple<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> () =
    inherit TypeShape<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6> ()
    interface IShapeTuple6 with
        member __.Accept v = v.Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> ()

type ITuple7Visitor<'R> =
    abstract Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> : unit -> 'R

type IShapeTuple7 =
    abstract Accept : ITuple7Visitor<'R> -> 'R

type ShapeTuple<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> () =
    inherit TypeShape<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7> ()
    interface IShapeTuple7 with
        member __.Accept v = v.Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> ()

type ITuple8Visitor<'R> =
    abstract Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'TRest> : unit -> 'R

type IShapeTuple8 =
    abstract Accept : ITuple8Visitor<'R> -> 'R

type ShapeTuple<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'TRest> () =
    inherit TypeShape<Tuple<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'TRest>> ()
    interface IShapeTuple8 with
        member __.Accept v = v.Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'TRest> ()

/////////// F# functions

type IFSharpFuncVisitor<'R> =
    abstract Visit<'Domain, 'CoDomain> : unit -> 'R

type IShapeFSharpFunc =
    abstract Accept : IFSharpFuncVisitor<'R> -> 'R

type ShapeFSharpFunc<'T, 'U> () =
    inherit TypeShape<'T -> 'U> ()
    interface IShapeFSharpFunc with
        member __.Accept v = v.Visit<'T, 'U> ()

/////////// System.Exception

type IExceptionVisitor<'R> =
    abstract Visit<'exn when 'exn :> exn> : unit -> 'R

type IShapeException =
    abstract Accept : IExceptionVisitor<'R> -> 'R

type ShapeException<'exn when 'exn :> exn> () =
    inherit TypeShape<'exn> ()
    interface IShapeException with
        member __.Accept v = v.Visit<'exn> ()


/////////////////////////////////////////////////
///////////// Section: Collections & IEnumerable

type IEnumerableVisitor<'R> =
    abstract Visit<'T> : unit -> 'R

type IShapeEnumerable =
    abstract Accept : IEnumerableVisitor<'R> -> 'R

type ShapeEnumerable<'T>() =
    inherit TypeShape<seq<'T>> ()
    interface IShapeEnumerable with
        member __.Accept v = v.Visit<'T> ()

type ICollectionVisitor<'R> =
    abstract Visit<'T> : unit -> 'R

type IShapeCollection =
    inherit IShapeEnumerable
    abstract Accept : ICollectionVisitor<'R> -> 'R

type ShapeCollection<'T>() =
    inherit TypeShape<ICollection<'T>>()
    interface IShapeCollection with
        member __.Accept v = v.Visit<'T> ()

    interface IShapeEnumerable with
        member __.Accept v = v.Visit<'T> ()

///////////// KeyValuePair

type IKeyValuePairVisitor<'R> =
    abstract Visit<'K, 'V> : unit -> 'R

type IShapeKeyValuePair =
    abstract Accept : IKeyValuePairVisitor<'R> -> 'R

type ShapeKeyValuePair<'K,'V> () =
    inherit TypeShape<KeyValuePair<'K,'V>> ()
    interface IShapeKeyValuePair with
        member __.Accept v = v.Visit<'K, 'V> ()

///////////// System.Array

type IArrayVisitor<'R> =
    abstract Visit<'T> : unit -> 'R

type IShapeArray =
    abstract Accept : IArrayVisitor<'R> -> 'R

type ShapeArray<'T>() =
    inherit TypeShape<'T []> ()
    interface IShapeArray with
        member __.Accept v = v.Visit<'T> ()

    interface IShapeCollection with
        member __.Accept v = v.Visit<'T> ()

    interface IShapeEnumerable with
        member __.Accept v = v.Visit<'T> ()

type IArray2DVisitor<'R> =
    abstract Visit<'T> : unit -> 'R

type IShapeArray2D =
    abstract Accept : IArray2DVisitor<'R> -> 'R

type ShapeArray2D<'T>() =
    inherit TypeShape<'T [,]> ()
    interface IShapeArray2D with
        member __.Accept v = v.Visit<'T> ()


type IArray3DVisitor<'R> =
    abstract Visit<'T> : unit -> 'R

type IShapeArray3D =
    abstract Accept : IArray3DVisitor<'R> -> 'R

type ShapeArray3D<'T>() =
    inherit TypeShape<'T [,,]> ()
    interface IShapeArray3D with
        member __.Accept v = v.Visit<'T> ()


type IArray4DVisitor<'R> =
    abstract Visit<'T> : unit -> 'R

type IShapeArray4D =
    abstract Accept : IArray4DVisitor<'R> -> 'R

type ShapeArray4D<'T>() =
    inherit TypeShape<'T [,,,]> ()
    interface IShapeArray4D with
        member __.Accept v = v.Visit<'T> ()

///////////// System.Collections.List

type IResizeArrayVisitor<'R> =
    abstract Visit<'T> : unit -> 'R

type IShapeResizeArray =
    abstract Accept : IResizeArrayVisitor<'R> -> 'R

type ShapeResizeArray<'T> () =
    inherit TypeShape<ResizeArray<'T>> ()
    interface IShapeResizeArray with
        member __.Accept v = v.Visit<'T> ()


///////////// System.Collections.Dictionary

type IDictionaryVisitor<'R> =
    abstract Visit<'K, 'V when 'K : equality> : unit -> 'R

type IShapeDictionary =
    abstract Accept : IDictionaryVisitor<'R> -> 'R

type ShapeDictionary<'K, 'V when 'K : equality> () =
    inherit TypeShape<Dictionary<'K, 'V>> ()
    interface IShapeDictionary with
        member __.Accept v = v.Visit<'K, 'V> ()
    interface IShapeEnumerable with
        member __.Accept v = v.Visit<KeyValuePair<'K, 'V>> ()
    interface IShapeCollection with
        member __.Accept v = v.Visit<KeyValuePair<'K, 'V>> ()

///////////// System.Collections.HashSet

type IHashSetVisitor<'R> =
    abstract Visit<'T when 'T : equality> : unit -> 'R

type IShapeHashSet =
    abstract Accept : IHashSetVisitor<'R> -> 'R

type ShapeHashSet<'T when 'T : equality> () =
    inherit TypeShape<HashSet<'T>> ()
    interface IShapeHashSet with
        member __.Accept v = v.Visit<'T> ()
    interface IShapeEnumerable with
        member __.Accept v = v.Visit<'T> ()
    interface IShapeCollection with
        member __.Accept v = v.Visit<'T> ()

///////////// F# Set

type IFSharpSetVisitor<'R> =
    abstract Visit<'T when 'T : comparison> : unit -> 'R

type IShapeFSharpSet =
    abstract Accept : IFSharpSetVisitor<'R> -> 'R

type ShapeFSharpSet<'T when 'T : comparison> () =
    inherit TypeShape<Set<'T>> ()
    interface IShapeFSharpSet with
        member __.Accept v = v.Visit<'T> ()
    interface IShapeEnumerable with
        member __.Accept v = v.Visit<'T> ()
    interface IShapeCollection with
        member __.Accept v = v.Visit<'T> ()

///////////// F# Map

type IFSharpMapVisitor<'R> =
    abstract Visit<'K, 'V when 'K : comparison> : unit -> 'R

type IShapeFSharpMap =
    abstract Accept : IFSharpMapVisitor<'R> -> 'R

type ShapeFSharpMap<'K, 'V when 'K : comparison> () =
    inherit TypeShape<Map<'K,'V>> ()
    interface IShapeFSharpMap with
        member __.Accept v = v.Visit<'K, 'V>()
    interface IShapeEnumerable with
        member __.Accept v = v.Visit<KeyValuePair<'K, 'V>> ()
    interface IShapeCollection with
        member __.Accept v = v.Visit<KeyValuePair<'K, 'V>> ()

//////////////////////////////////
///////////// Section: F# Records

type private RecordInfo =
    {
        RProperties : PropertyInfo[]
        RCtor : obj[] -> obj
    }

type IShapeFSharpRecord =
    abstract IsFSharpRef : bool
    abstract Properties : PropertyInfo list

[<AbstractClass>]
type ShapeFSharpRecord<'Record> internal () =
    inherit TypeShape<'Record>()
    abstract IsFSharpRef : bool
    abstract Properties : PropertyInfo list
    interface IShapeFSharpRecord with
        member __.IsFSharpRef = __.IsFSharpRef
        member __.Properties = __.Properties

type IShapeFSharpRecord1 =
    inherit IShapeFSharpRecord
    abstract Accept<'R> : IFSharpRecord1Visitor<'R> -> 'R

and IFSharpRecord1Visitor<'R> =
    abstract Visit<'Record, 'Field1> : ShapeFSharpRecord<'Record, 'Field1> -> 'R

and ShapeFSharpRecord<'Record, 'Field1> private (info : RecordInfo) =
    inherit ShapeFSharpRecord<'Record>()
    override __.IsFSharpRef = false
    override __.Properties = info.RProperties |> Array.toList

    member __.Ctor(f1 : 'Field1) = info.RCtor [|f1|] :?> 'Record
    member __.Proj1(record : 'Record) = info.RProperties.[0].GetValue(record, null) :?> 'Field1

    interface IShapeFSharpRecord1 with
        member s.Accept v = v.Visit<'Record, 'Field1> s

type IShapeFSharpRecord2 =
    inherit IShapeFSharpRecord
    abstract Accept<'R> : IFSharpRecord2Visitor<'R> -> 'R

and IFSharpRecord2Visitor<'R> =
    abstract Visit<'Record, 'Field1, 'Field2> : ShapeFSharpRecord<'Record, 'Field1, 'Field2> -> 'R

and ShapeFSharpRecord<'Record, 'Field1, 'Field2> private (info : RecordInfo) =
    inherit ShapeFSharpRecord<'Record>()
    override __.IsFSharpRef = false
    override __.Properties = info.RProperties |> Array.toList

    member __.Ctor(f1 : 'Field1, f2 : 'Field2) = info.RCtor [|f1; f2|] :?> 'Record
    member __.Proj1(record : 'Record) = info.RProperties.[0].GetValue(record, null) :?> 'Field1
    member __.Proj2(record : 'Record) = info.RProperties.[1].GetValue(record, null) :?> 'Field2

    interface IShapeFSharpRecord2 with
        member s.Accept v = v.Visit<'Record, 'Field1, 'Field2> s

type IShapeFSharpRecord3 =
    inherit IShapeFSharpRecord
    abstract Accept<'R> : IFSharpRecord3Visitor<'R> -> 'R

and IFSharpRecord3Visitor<'R> =
    abstract Visit<'Record, 'Field1, 'Field2, 'Field3> : ShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3> -> 'R

and ShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3> private (info : RecordInfo) =
    inherit ShapeFSharpRecord<'Record>()
    override __.IsFSharpRef = false
    override __.Properties = info.RProperties |> Array.toList

    member __.Ctor(f1 : 'Field1, f2 : 'Field2, f3 : 'Field3) = info.RCtor [|f1; f2; f3|] :?> 'Record
    member __.Proj1(record : 'Record) = info.RProperties.[0].GetValue(record, null) :?> 'Field1
    member __.Proj2(record : 'Record) = info.RProperties.[1].GetValue(record, null) :?> 'Field2
    member __.Proj3(record : 'Record) = info.RProperties.[2].GetValue(record, null) :?> 'Field3

    interface IShapeFSharpRecord3 with
        member s.Accept v = v.Visit<'Record, 'Field1, 'Field2, 'Field3> s

type IShapeFSharpRecord4 =
    inherit IShapeFSharpRecord
    abstract Accept<'R> : IFSharpRecord4Visitor<'R> -> 'R

and IFSharpRecord4Visitor<'R> =
    abstract Visit<'Record, 'Field1, 'Field2, 'Field3, 'Field4> : ShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4> -> 'R

and ShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4> private (info : RecordInfo) =
    inherit ShapeFSharpRecord<'Record>()
    override __.IsFSharpRef = false
    override __.Properties = info.RProperties |> Array.toList

    member __.Ctor(f1 : 'Field1, f2 : 'Field2, f3 : 'Field3, f4 : 'Field4) = info.RCtor [|f1; f2; f3; f4|] :?> 'Record
    member __.Proj1(record : 'Record) = info.RProperties.[0].GetValue(record, null) :?> 'Field1
    member __.Proj2(record : 'Record) = info.RProperties.[1].GetValue(record, null) :?> 'Field2
    member __.Proj3(record : 'Record) = info.RProperties.[2].GetValue(record, null) :?> 'Field3
    member __.Proj4(record : 'Record) = info.RProperties.[3].GetValue(record, null) :?> 'Field4

    interface IShapeFSharpRecord4 with
        member s.Accept v = v.Visit<'Record, 'Field1, 'Field2, 'Field3, 'Field4> s

type IShapeFSharpRecord5 =
    inherit IShapeFSharpRecord
    abstract Accept<'R> : IFSharpRecord5Visitor<'R> -> 'R

and IFSharpRecord5Visitor<'R> =
    abstract Visit<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5> : ShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5> -> 'R

and ShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5> private (info : RecordInfo) =
    inherit ShapeFSharpRecord<'Record>()
    override __.IsFSharpRef = false
    override __.Properties = info.RProperties |> Array.toList

    member __.Ctor(f1 : 'Field1, f2 : 'Field2, f3 : 'Field3, f4 : 'Field4, f5 : 'Field5) = info.RCtor [|f1; f2; f3; f4; f5|] :?> 'Record
    member __.Proj1(record : 'Record) = info.RProperties.[0].GetValue(record, null) :?> 'Field1
    member __.Proj2(record : 'Record) = info.RProperties.[1].GetValue(record, null) :?> 'Field2
    member __.Proj3(record : 'Record) = info.RProperties.[2].GetValue(record, null) :?> 'Field3
    member __.Proj4(record : 'Record) = info.RProperties.[3].GetValue(record, null) :?> 'Field4
    member __.Proj5(record : 'Record) = info.RProperties.[4].GetValue(record, null) :?> 'Field5

    interface IShapeFSharpRecord5 with
        member s.Accept v = v.Visit<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5> s

type IShapeFSharpRecord6 =
    inherit IShapeFSharpRecord
    abstract Accept<'R> : IFSharpRecord6Visitor<'R> -> 'R

and IFSharpRecord6Visitor<'R> =
    abstract Visit<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5, 'Field6> : ShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5, 'Field6> -> 'R

and ShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5, 'Field6> private (info : RecordInfo) =
    inherit ShapeFSharpRecord<'Record>()
    override __.IsFSharpRef = false
    override __.Properties = info.RProperties |> Array.toList

    member __.Ctor(f1 : 'Field1, f2 : 'Field2, f3 : 'Field3, f4 : 'Field4, f5 : 'Field5, f6 : 'Field6) = info.RCtor [|f1; f2; f3; f4; f5 ; f6|] :?> 'Record
    member __.Proj1(record : 'Record) = info.RProperties.[0].GetValue(record, null) :?> 'Field1
    member __.Proj2(record : 'Record) = info.RProperties.[1].GetValue(record, null) :?> 'Field2
    member __.Proj3(record : 'Record) = info.RProperties.[2].GetValue(record, null) :?> 'Field3
    member __.Proj4(record : 'Record) = info.RProperties.[3].GetValue(record, null) :?> 'Field4
    member __.Proj5(record : 'Record) = info.RProperties.[4].GetValue(record, null) :?> 'Field5
    member __.Proj6(record : 'Record) = info.RProperties.[5].GetValue(record, null) :?> 'Field6

    interface IShapeFSharpRecord6 with
        member s.Accept v = v.Visit<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5, 'Field6> s

type IShapeFSharpRecord7 =
    inherit IShapeFSharpRecord
    abstract Accept<'R> : IFSharpRecord7Visitor<'R> -> 'R

and IFSharpRecord7Visitor<'R> =
    abstract Visit<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5, 'Field6, 'Field67> : ShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5, 'Field6, 'Field67> -> 'R

and ShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5, 'Field6, 'Field67> private (info : RecordInfo) =
    inherit ShapeFSharpRecord<'Record>()
    override __.IsFSharpRef = false
    override __.Properties = info.RProperties |> Array.toList

    member __.Ctor(f1 : 'Field1, f2 : 'Field2, f3 : 'Field3, f4 : 'Field4, f5 : 'Field5, f6 : 'Field6, f7 : 'Field67) = 
        info.RCtor [|f1; f2; f3; f4; f5; f6; f7|] :?> 'Record

    member __.Proj1(record : 'Record) = info.RProperties.[0].GetValue(record, null) :?> 'Field1
    member __.Proj2(record : 'Record) = info.RProperties.[1].GetValue(record, null) :?> 'Field2
    member __.Proj3(record : 'Record) = info.RProperties.[2].GetValue(record, null) :?> 'Field3
    member __.Proj4(record : 'Record) = info.RProperties.[3].GetValue(record, null) :?> 'Field4
    member __.Proj5(record : 'Record) = info.RProperties.[4].GetValue(record, null) :?> 'Field5
    member __.Proj6(record : 'Record) = info.RProperties.[5].GetValue(record, null) :?> 'Field6
    member __.Proj7(record : 'Record) = info.RProperties.[6].GetValue(record, null) :?> 'Field67

    interface IShapeFSharpRecord7 with
        member s.Accept v = v.Visit<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5, 'Field6, 'Field67> s

///////////// F# ref

type IShapeFSharpRef =
    inherit IShapeFSharpRecord
    abstract Accept : IFSharpRefVisitor<'R> -> 'R

and IFSharpRefVisitor<'R> =
    abstract Visit<'T> : unit -> 'R

type ShapeFSharpRef<'T>() =
    inherit TypeShape<'T ref> ()
    interface IShapeFSharpRef with
        member x.IsFSharpRef = true
        member x.Properties = typeof<'T ref>.GetProperties() |> Array.toList
        member x.Accept v = v.Visit<'T> ()
        

/////////////////////////////////
///////////// Section: F# Unions

type private CaseInfo = 
    {
        CaseInfo : UnionCaseInfo
        PayloadType : Type
        UCtor : obj -> obj
        UProj : obj -> obj
    }

type private UnionInfo =
    {
        IsChoiceType : bool
        TagReader : obj -> int
        Cases : CaseInfo []
    }
with
    member __.UnionCaseInfo =
        __.Cases |> Seq.map (fun u -> u.CaseInfo) |> Seq.toList

type IShapeFSharpUnion =
    abstract IsFSharpOption : bool
    abstract IsFSharpChoice : bool
    abstract IsFSharpList : bool
    abstract GetTag : obj -> int
    abstract UnionCaseInfo : UnionCaseInfo list

[<AbstractClass>]
type ShapeFSharpUnion<'U> internal () =
    inherit TypeShape<'U>()
    abstract GetTag : 'U -> int
    abstract UnionCaseInfo : UnionCaseInfo list
    abstract IsFSharpOption : bool
    abstract IsFSharpChoice : bool
    abstract IsFSharpList : bool
    interface IShapeFSharpUnion with
        member __.GetTag obj = __.GetTag (obj :?> 'U)
        member __.UnionCaseInfo = __.UnionCaseInfo
        member __.IsFSharpOption = __.IsFSharpOption
        member __.IsFSharpChoice = __.IsFSharpChoice
        member __.IsFSharpList = __.IsFSharpList

type IFSharpUnion1Visitor<'R> =
    abstract Visit<'Union, 'Case1> : ShapeFSharpUnion<'Union, 'Case1> -> 'R

and IShapeFSharpUnion1 =
    inherit IShapeFSharpUnion
    abstract Accept : IFSharpUnion1Visitor<'R> -> 'R

and ShapeFSharpUnion<'Union, 'Case1> private (info : UnionInfo) =
    inherit ShapeFSharpUnion<'Union>()
    member __.Project (u : 'Union) = 
        let { UProj = proj } = info.Cases.[0]
        proj u :?> 'Case1

    member __.Ctor1 (c : 'Case1) = 
        let { UCtor = ctor } = info.Cases.[0]
        ctor c :?> 'Union

    override __.GetTag (u : 'Union) = info.TagReader (u :> _)
    override __.UnionCaseInfo = info.UnionCaseInfo
    override __.IsFSharpOption = false
    override __.IsFSharpList = false
    override __.IsFSharpChoice = info.IsChoiceType
    interface IShapeFSharpUnion1 with
        member self.Accept v = v.Visit<'Union, 'Case1> self

type IFSharpUnion2Visitor<'R> =
    abstract Visit<'U, 'Case1, 'Case2> : ShapeFSharpUnion<'U, 'Case1, 'Case2> -> 'R

and IShapeFSharpUnion2 =
    abstract Accept : IFSharpUnion2Visitor<'R> -> 'R

and ShapeFSharpUnion<'U, 'Case1, 'Case2> private (info : UnionInfo) =
    inherit ShapeFSharpUnion<'U>()
    member __.Project (u : 'U) =
        if info.IsChoiceType then u :> obj :?> _ else 
        let tag = info.TagReader (u :> _)
        let { UProj = proj } = info.Cases.[tag]
        let value = proj u
        match tag with
        | 0 -> Choice1Of2(value :?> 'Case1)
        | _ -> Choice2Of2(value :?> 'Case2)

    member __.Ctor1 (c : 'Case1) =
        let { UCtor = ctor } = info.Cases.[0]
        ctor c :?> 'U

    member __.Ctor2 (c : 'Case2) =
        let { UCtor = ctor } = info.Cases.[1]
        ctor c :?> 'U

    override __.GetTag (u : 'U) = info.TagReader (u :> _)
    override __.UnionCaseInfo = info.UnionCaseInfo
    override __.IsFSharpOption = false
    override __.IsFSharpList = false
    override __.IsFSharpChoice = info.IsChoiceType

    interface IShapeFSharpUnion2 with
        member self.Accept(v : IFSharpUnion2Visitor<'R>) = v.Visit<'U, 'Case1, 'Case2> self

type IFSharpUnion3Visitor<'R> =
    abstract Visit<'U, 'Case1, 'Case2, 'Case3> : ShapeFSharpUnion<'U, 'Case1, 'Case2, 'Case3> -> 'R

and IShapeFSharpUnion3 =
    abstract Accept : IFSharpUnion3Visitor<'R> -> 'R

and ShapeFSharpUnion<'U, 'Case1, 'Case2, 'Case3> private (info : UnionInfo) =
    inherit ShapeFSharpUnion<'U>()
    member __.Project (u : 'U) =
        if info.IsChoiceType then u :> obj :?> _ else 
        let tag = info.TagReader (u :> _)
        let { UProj = proj } = info.Cases.[tag]
        let value = proj u
        match tag with
        | 0 -> Choice1Of3(value :?> 'Case1)
        | 1 -> Choice2Of3(value :?> 'Case2)
        | _ -> Choice3Of3(value :?> 'Case3)

    member __.Ctor1 (c : 'Case1) =
        let { UCtor = ctor } = info.Cases.[0]
        ctor c :?> 'U

    member __.Ctor2 (c : 'Case2) =
        let { UCtor = ctor } = info.Cases.[1]
        ctor c :?> 'U

    member __.Ctor3 (c : 'Case3) =
        let { UCtor = ctor } = info.Cases.[2]
        ctor c :?> 'U

    override __.GetTag (u : 'U) = info.TagReader (u :> _)
    override __.UnionCaseInfo = info.UnionCaseInfo
    override __.IsFSharpOption = false
    override __.IsFSharpList = false
    override __.IsFSharpChoice = info.IsChoiceType

    interface IShapeFSharpUnion3 with
        member self.Accept(v : IFSharpUnion3Visitor<'R>) = 
            v.Visit<'U, 'Case1, 'Case2, 'Case3> self

type IFSharpUnion4Visitor<'R> =
    abstract Visit<'U, 'Case1, 'Case2, 'Case3, 'Case4> : ShapeFSharpUnion<'U, 'Case1, 'Case2, 'Case3, 'Case4> -> 'R

and IShapeFSharpUnion4 =
    abstract Accept : IFSharpUnion4Visitor<'R> -> 'R

and ShapeFSharpUnion<'U, 'Case1, 'Case2, 'Case3, 'Case4> private (info : UnionInfo) =
    inherit ShapeFSharpUnion<'U>()
    member __.Project (u : 'U) =
        if info.IsChoiceType then u :> obj :?> _ else 
        let tag = info.TagReader (u :> _)
        let { UProj = proj } = info.Cases.[tag]
        let value = proj u
        match tag with
        | 0 -> Choice1Of4(value :?> 'Case1)
        | 1 -> Choice2Of4(value :?> 'Case2)
        | 2 -> Choice3Of4(value :?> 'Case3)
        | _ -> Choice4Of4(value :?> 'Case4)

    member __.Ctor1 (c : 'Case1) =
        let { UCtor = ctor } = info.Cases.[0]
        ctor c :?> 'U

    member __.Ctor2 (c : 'Case2) =
        let { UCtor = ctor } = info.Cases.[1]
        ctor c :?> 'U

    member __.Ctor3 (c : 'Case3) =
        let { UCtor = ctor } = info.Cases.[2]
        ctor c :?> 'U

    member __.Ctor4 (c : 'Case4) =
        let { UCtor = ctor } = info.Cases.[3]
        ctor c :?> 'U

    override __.GetTag (u : 'U) = info.TagReader (u :> _)
    override __.UnionCaseInfo = info.UnionCaseInfo
    override __.IsFSharpChoice = info.IsChoiceType
    override __.IsFSharpOption = false
    override __.IsFSharpList = false

    interface IShapeFSharpUnion4 with
        member self.Accept(v : IFSharpUnion4Visitor<'R>) = 
            v.Visit<'U, 'Case1, 'Case2, 'Case3, 'Case4> self

type IFSharpUnion5Visitor<'R> =
    abstract Visit<'U, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5> : ShapeFSharpUnion<'U, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5> -> 'R

and IShapeFSharpUnion5 =
    abstract Accept : IFSharpUnion5Visitor<'R> -> 'R

and ShapeFSharpUnion<'U, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5> private (info : UnionInfo) =
    inherit ShapeFSharpUnion<'U>()
    member __.Project (u : 'U) =
        if info.IsChoiceType then u :> obj :?> _ else 
        let tag = info.TagReader (u :> _)
        let { UProj = proj } = info.Cases.[tag]
        let value = proj u
        match tag with
        | 0 -> Choice1Of5(value :?> 'Case1)
        | 1 -> Choice2Of5(value :?> 'Case2)
        | 2 -> Choice3Of5(value :?> 'Case3)
        | 3 -> Choice4Of5(value :?> 'Case4)
        | _ -> Choice5Of5(value :?> 'Case5)

    member __.Ctor1 (c : 'Case1) =
        let { UCtor = ctor } = info.Cases.[0]
        ctor c :?> 'U

    member __.Ctor2 (c : 'Case2) =
        let { UCtor = ctor } = info.Cases.[1]
        ctor c :?> 'U

    member __.Ctor3 (c : 'Case3) =
        let { UCtor = ctor } = info.Cases.[2]
        ctor c :?> 'U

    member __.Ctor4 (c : 'Case4) =
        let { UCtor = ctor } = info.Cases.[3]
        ctor c :?> 'U

    member __.Ctor5 (c : 'Case5) =
        let { UCtor = ctor } = info.Cases.[3]
        ctor c :?> 'U

    override __.GetTag (u : 'U) = info.TagReader (u :> _)
    override __.UnionCaseInfo = info.UnionCaseInfo
    override __.IsFSharpChoice = info.IsChoiceType
    override __.IsFSharpOption = false
    override __.IsFSharpList = false

    interface IShapeFSharpUnion5 with
        member self.Accept(v : IFSharpUnion5Visitor<'R>) = 
            v.Visit<'U, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5> self

type IFSharpUnion6Visitor<'R> =
    abstract Visit<'U, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6> : ShapeFSharpUnion<'U, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6> -> 'R

and IShapeFSharpUnion6 =
    abstract Accept : IFSharpUnion6Visitor<'R> -> 'R

and ShapeFSharpUnion<'U, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6> private (info : UnionInfo) =
    inherit ShapeFSharpUnion<'U>()
    member __.Project (u : 'U) =
        if info.IsChoiceType then u :> obj :?> _ else 
        let tag = info.TagReader (u :> _)
        let { UProj = proj } = info.Cases.[tag]
        let value = proj u
        match tag with
        | 0 -> Choice1Of6(value :?> 'Case1)
        | 1 -> Choice2Of6(value :?> 'Case2)
        | 2 -> Choice3Of6(value :?> 'Case3)
        | 3 -> Choice4Of6(value :?> 'Case4)
        | 4 -> Choice5Of6(value :?> 'Case5)
        | _ -> Choice6Of6(value :?> 'Case6)

    member __.Ctor1 (c : 'Case1) =
        let { UCtor = ctor } = info.Cases.[0]
        ctor c :?> 'U

    member __.Ctor2 (c : 'Case2) =
        let { UCtor = ctor } = info.Cases.[1]
        ctor c :?> 'U

    member __.Ctor3 (c : 'Case3) =
        let { UCtor = ctor } = info.Cases.[2]
        ctor c :?> 'U

    member __.Ctor4 (c : 'Case4) =
        let { UCtor = ctor } = info.Cases.[3]
        ctor c :?> 'U

    member __.Ctor5 (c : 'Case5) =
        let { UCtor = ctor } = info.Cases.[3]
        ctor c :?> 'U

    member __.Ctor6 (c : 'Case6) =
        let { UCtor = ctor } = info.Cases.[3]
        ctor c :?> 'U

    override __.GetTag (u : 'U) = info.TagReader (u :> _)
    override __.UnionCaseInfo = info.UnionCaseInfo
    override __.IsFSharpChoice = info.IsChoiceType
    override __.IsFSharpOption = false
    override __.IsFSharpList = false

    interface IShapeFSharpUnion6 with
        member self.Accept(v : IFSharpUnion6Visitor<'R>) = 
            v.Visit<'U, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6> self

type IFSharpUnion7Visitor<'R> =
    abstract Visit<'U, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6, 'Case7> : ShapeFSharpUnion<'U, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6, 'Case7> -> 'R

and IShapeFSharpUnion7 =
    abstract Accept : IFSharpUnion7Visitor<'R> -> 'R

and ShapeFSharpUnion<'U, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6, 'Case7> private (info : UnionInfo) =
    inherit ShapeFSharpUnion<'U>()
    member __.Project (u : 'U) =
        if info.IsChoiceType then u :> obj :?> _ else 
        let tag = info.TagReader (u :> _)
        let { UProj = proj } = info.Cases.[tag]
        let value = proj u
        match tag with
        | 0 -> Choice1Of7(value :?> 'Case1)
        | 1 -> Choice2Of7(value :?> 'Case2)
        | 2 -> Choice3Of7(value :?> 'Case3)
        | 3 -> Choice4Of7(value :?> 'Case4)
        | 4 -> Choice5Of7(value :?> 'Case5)
        | 5 -> Choice6Of7(value :?> 'Case6)
        | _ -> Choice7Of7(value :?> 'Case7)

    member __.Ctor1 (c : 'Case1) =
        let { UCtor = ctor } = info.Cases.[0]
        ctor c :?> 'U

    member __.Ctor2 (c : 'Case2) =
        let { UCtor = ctor } = info.Cases.[1]
        ctor c :?> 'U

    member __.Ctor3 (c : 'Case3) =
        let { UCtor = ctor } = info.Cases.[2]
        ctor c :?> 'U

    member __.Ctor4 (c : 'Case4) =
        let { UCtor = ctor } = info.Cases.[3]
        ctor c :?> 'U

    member __.Ctor5 (c : 'Case5) =
        let { UCtor = ctor } = info.Cases.[3]
        ctor c :?> 'U

    member __.Ctor6 (c : 'Case6) =
        let { UCtor = ctor } = info.Cases.[3]
        ctor c :?> 'U

    member __.Ctor7 (c : 'Case7) =
        let { UCtor = ctor } = info.Cases.[3]
        ctor c :?> 'U

    override __.GetTag (u : 'U) = info.TagReader (u :> _)
    override __.UnionCaseInfo = info.UnionCaseInfo
    override __.IsFSharpChoice = info.IsChoiceType
    override __.IsFSharpOption = false
    override __.IsFSharpList = false

    interface IShapeFSharpUnion7 with
        member self.Accept(v : IFSharpUnion7Visitor<'R>) = 
            v.Visit<'U, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6, 'Case7> self

///////////// F# option

type IFSharpOptionVisitor<'R> =
    abstract Visit<'T> : unit -> 'R

type IShapeFSharpOption =
    inherit IShapeFSharpUnion
    abstract Accept : IFSharpOptionVisitor<'R> -> 'R

type ShapeFSharpOption<'T> () =
    inherit TypeShape<'T option> ()
    let ucis = FSharpType.GetUnionCases typeof<'T option> |> Array.toList
    interface IShapeFSharpOption with
        member __.GetTag t = match t :?> 'T option with None -> 0 | _ -> 1
        member __.IsFSharpChoice = false
        member __.IsFSharpList = false
        member __.IsFSharpOption = true
        member __.UnionCaseInfo = ucis
        member __.Accept v = v.Visit<'T> ()

///////////// F# List

type IFSharpListVisitor<'R> =
    abstract Visit<'T> : unit -> 'R

type IShapeFSharpList =
    inherit IShapeCollection
    inherit IShapeFSharpUnion
    abstract Accept : IFSharpListVisitor<'R> -> 'R

type ShapeFSharpList<'T> () =
    inherit TypeShape<'T list> ()
    let ucis = FSharpType.GetUnionCases typeof<'T list> |> Array.toList
    interface IShapeFSharpList with
        member __.Accept v = v.Visit<'T> ()
    interface IShapeEnumerable with
        member __.Accept v = v.Visit<'T> ()
    interface IShapeCollection with
        member __.Accept v = v.Visit<'T> ()
    interface IShapeFSharpUnion with
        member x.GetTag a = match a :?> 'T list with [] -> 0 | _ -> 1
        member x.IsFSharpChoice = false
        member x.IsFSharpList = true
        member x.IsFSharpOption = false
        member x.UnionCaseInfo = ucis

////////////////////////////////////////////
///////////// Section: TypeShape resolution

exception UnsupportedShape of Type:Type
 with
    override __.Message = sprintf "Unsupported TypeShape '%O'" __.Type

module private TypeShapeImpl =

    open System.Reflection
    open Microsoft.FSharp.Reflection

    let allMembers =
        BindingFlags.NonPublic ||| BindingFlags.Public |||
            BindingFlags.Instance ||| BindingFlags.Static |||
                BindingFlags.FlattenHierarchy

    // typedefof does not work properly with 'enum' constraints
    let getGenericEnumType () = 
        typeof<ShapeEnum<BindingFlags,int>>.GetGenericTypeDefinition()

    let activateArgs (gt : Type) (tp : Type []) (args : obj[]) =
        let ti = gt.MakeGenericType tp
        let ctypes = args |> Array.map (fun o -> o.GetType())
        let ctor = ti.GetConstructor(allMembers, null, CallingConventions.Standard, ctypes, [||])
        ctor.Invoke args :?> TypeShape

    let activate (gt : Type) (tp : Type []) = activateArgs gt tp [||]
    let activate1 (gt : Type) (tp : Type) = activate gt [|tp|]
    let activate2 (gt : Type) (p1 : Type) (p2 : Type) = activate gt [|p1 ; p2|]

    let canon = Type.GetType("System.__Canon")

    /// correctly resolves if type is assignable to interface
    let rec private isAssignableFrom (interfaceTy : Type) (ty : Type) =
        let proj (t : Type) = t.Assembly, t.Namespace, t.Name, t.MetadataToken
        if interfaceTy = ty then true
        elif ty.GetInterfaces() |> Array.exists(fun if0 -> proj if0 = proj interfaceTy) then true
        else
            match ty.BaseType with
            | null -> false
            | bt -> isAssignableFrom interfaceTy bt
        
    /// use reflection to bootstrap a shape instance
    let resolveTypeShape (t : Type) : TypeShape =
        if t.IsGenericTypeDefinition then raise <| UnsupportedShape t
        elif t.IsGenericParameter then raise <| UnsupportedShape t
        elif t = canon then raise <| UnsupportedShape t
        elif t.IsEnum then 
            activate2 (getGenericEnumType()) t <| Enum.GetUnderlyingType t

        elif t.IsArray then
            let et = t.GetElementType()
            match t.GetArrayRank() with
            | 1 -> activate1 typedefof<ShapeArray<_>> et
            | 2 -> activate1 typedefof<ShapeArray2D<_>> et
            | 3 -> activate1 typedefof<ShapeArray3D<_>> et
            | 4 -> activate1 typedefof<ShapeArray4D<_>> et
            | _ -> raise <| UnsupportedShape t

        elif FSharpType.IsTuple t then
            let gas = t.GetGenericArguments()
            match gas.Length with
            | 1 -> activate typedefof<ShapeTuple<_>> gas
            | 2 -> activate typedefof<ShapeTuple<_,_>> gas
            | 3 -> activate typedefof<ShapeTuple<_,_,_>> gas
            | 4 -> activate typedefof<ShapeTuple<_,_,_,_>> gas
            | 5 -> activate typedefof<ShapeTuple<_,_,_,_,_>> gas
            | 6 -> activate typedefof<ShapeTuple<_,_,_,_,_,_>> gas
            | 7 -> activate typedefof<ShapeTuple<_,_,_,_,_,_,_>> gas
            | 8 -> activate typedefof<ShapeTuple<_,_,_,_,_,_,_,_>> gas
            | _ -> raise <| UnsupportedShape t

        elif isAssignableFrom typeof<exn> t then
            activate1 typedefof<ShapeException<_>> t

        elif FSharpType.IsFunction t then
            let d,c = FSharpType.GetFunctionElements t
            activate2 typedefof<ShapeFSharpFunc<_,_>> d c


        elif FSharpType.IsRecord(t, allMembers) then
            let genTy = 
                if t.IsGenericType then Some(t.GetGenericTypeDefinition())
                else None

            if genTy = Some typedefof<_ ref> then 
                activate typedefof<ShapeFSharpRef<_>> (t.GetGenericArguments())
            else

            let ctor = FSharpValue.PreComputeRecordConstructor(t, allMembers)
            let properties = FSharpType.GetRecordFields(t, allMembers)
            let info = {
                RCtor = ctor
                RProperties = properties
            }

            match properties with
            | [|p1|] -> activateArgs typedefof<ShapeFSharpRecord<_,_>> [|t;p1.PropertyType|] [|info|]
            | [|p1;p2|] -> activateArgs typedefof<ShapeFSharpRecord<_,_,_>> [|t;p1.PropertyType;p2.PropertyType|] [|info|]
            | [|p1;p2;p3|] -> activateArgs typedefof<ShapeFSharpRecord<_,_,_,_>> [|t;p1.PropertyType;p2.PropertyType;p3.PropertyType|] [|info|]
            | [|p1;p2;p3;p4|] -> activateArgs typedefof<ShapeFSharpRecord<_,_,_,_,_>> [|t;p1.PropertyType;p2.PropertyType;p3.PropertyType;p4.PropertyType|] [|info|]
            | [|p1;p2;p3;p4;p5|] -> activateArgs typedefof<ShapeFSharpRecord<_,_,_,_,_,_>> [|t;p1.PropertyType;p2.PropertyType;p3.PropertyType;p4.PropertyType;p5.PropertyType|] [|info|]
            | [|p1;p2;p3;p4;p5;p6|] -> activateArgs typedefof<ShapeFSharpRecord<_,_,_,_,_,_,_>> [|t;p1.PropertyType;p2.PropertyType;p3.PropertyType;p4.PropertyType;p5.PropertyType;p6.PropertyType|] [|info|]
            | [|p1;p2;p3;p4;p5;p6;p7|] -> activateArgs typedefof<ShapeFSharpRecord<_,_,_,_,_,_,_,_>> [|t;p1.PropertyType;p2.PropertyType;p3.PropertyType;p4.PropertyType;p5.PropertyType;p6.PropertyType;p7.PropertyType|] [|info|]
            | _ -> activate1 typedefof<TypeShape<_>> t

        elif FSharpType.IsUnion(t, allMembers) then
            let genTy = 
                if t.IsGenericType then Some(t.GetGenericTypeDefinition())
                else None

            if genTy = Some(typedefof<_ list>) then
                activate typedefof<ShapeFSharpList<_>> (t.GetGenericArguments())
            elif genTy = Some(typedefof<_ option>) then
                activate typedefof<ShapeFSharpOption<_>> (t.GetGenericArguments())
            else
                let isChoice =
                    Option.isSome genTy &&
                    t.Name.StartsWith "FSharpChoice" && 
                    t.Namespace = "Microsoft.FSharp.Core" && 
                    t.Assembly = typeof<int option>.Assembly

                let mkCaseInfo (uci : UnionCaseInfo) =
                    let fields = uci.GetFields()
                    let uctor = FSharpValue.PreComputeUnionConstructor(uci, allMembers)
                    match fields with
                    | [||] -> 
                        { CaseInfo = uci ; PayloadType = typeof<unit> ;
                            UCtor = (fun _ -> uctor [||]) ;
                            UProj = (fun _ -> () :> _) }

                    | [|field|] -> 
                        { CaseInfo = uci ; PayloadType = field.PropertyType ;
                            UCtor = (fun v -> uctor [|v|]) ;
                            UProj = (fun u -> field.GetValue(u, null)) }
                    | _ ->
                        let tupleType = fields |> Array.map (fun f -> f.PropertyType) |> FSharpType.MakeTupleType
                        let uReader = FSharpValue.PreComputeUnionReader(uci, allMembers)
                        let tupleCtor = FSharpValue.PreComputeTupleConstructor tupleType
                        let tupleReader = FSharpValue.PreComputeTupleReader tupleType
                        { CaseInfo = uci ; PayloadType = tupleType ;
                            UCtor = tupleReader >> uctor ;
                            UProj = uReader >> tupleCtor }

                let tagReader = FSharpValue.PreComputeUnionTagReader(t, allMembers)
                let ucis = FSharpType.GetUnionCases(t, allMembers)
                let caseInfo = ucis |> Array.map mkCaseInfo
                let unionInfo = { IsChoiceType = isChoice ; TagReader = tagReader ; Cases = caseInfo }
                match caseInfo with
                | [|c1|] -> activateArgs typedefof<ShapeFSharpUnion<_,_>> [|t ; c1.PayloadType|] [|unionInfo|]
                | [|c1;c2|] -> activateArgs typedefof<ShapeFSharpUnion<_,_,_>> [|t ; c1.PayloadType ; c2.PayloadType|] [|unionInfo|]
                | [|c1;c2;c3|] -> activateArgs typedefof<ShapeFSharpUnion<_,_,_,_>> [|t ; c1.PayloadType ; c2.PayloadType ; c3.PayloadType|] [|unionInfo|]
                | [|c1;c2;c3;c4|] -> activateArgs typedefof<ShapeFSharpUnion<_,_,_,_,_>> [|t ; c1.PayloadType ; c2.PayloadType ; c3.PayloadType ; c4.PayloadType|] [|unionInfo|]
                | [|c1;c2;c3;c4;c5|] -> activateArgs typedefof<ShapeFSharpUnion<_,_,_,_,_,_>> [|t ; c1.PayloadType ; c2.PayloadType ; c3.PayloadType ; c4.PayloadType ; c5.PayloadType|] [|unionInfo|]
                | [|c1;c2;c3;c4;c5;c6|] -> activateArgs typedefof<ShapeFSharpUnion<_,_,_,_,_,_,_>> [|t ; c1.PayloadType ; c2.PayloadType ; c3.PayloadType ; c4.PayloadType ; c5.PayloadType ; c6.PayloadType|] [|unionInfo|]
                | [|c1;c2;c3;c4;c5;c6;c7|] -> activateArgs typedefof<ShapeFSharpUnion<_,_,_,_,_,_,_,_>> [|t ; c1.PayloadType ; c2.PayloadType ; c3.PayloadType ; c4.PayloadType ; c5.PayloadType ; c6.PayloadType ; c7.PayloadType|] [|unionInfo|]
                | _ -> activate1 typedefof<TypeShape<_>> t

        elif t.IsGenericType then
            let gt = t.GetGenericTypeDefinition()
            let gas = t.GetGenericArguments()

            if gt = typedefof<System.Nullable<_>> then
                activate typedefof<ShapeNullable<_>> gas
            elif gt = typedefof<System.Collections.Generic.Dictionary<_,_>> then
                activate typedefof<ShapeDictionary<_,_>> gas
            elif gt = typedefof<System.Collections.Generic.HashSet<_>> then
                activate typedefof<ShapeHashSet<_>> gas
            elif gt = typedefof<System.Collections.Generic.List<_>> then
                activate typedefof<ShapeResizeArray<_>> gas
            elif gt = typedefof<Map<_,_>> then
                activate typedefof<ShapeFSharpMap<_,_>> gas
            elif gt = typedefof<Set<_>> then
                activate typedefof<ShapeFSharpSet<_>> gas
            elif gt = typedefof<KeyValuePair<_,_>> then
                activate typedefof<ShapeKeyValuePair<_,_>> gas
            elif isAssignableFrom typedefof<ICollection<_>> gt then
                activate typedefof<ShapeCollection<_>> gas
            elif isAssignableFrom typedefof<IEnumerable<_>> gt then
                activate typedefof<ShapeEnumerable<_>> gas
            else
                activate1 typedefof<TypeShape<_>> t
        else 
            activate1 typedefof<TypeShape<_>> t


    let dict = new System.Collections.Concurrent.ConcurrentDictionary<Type, TypeShape>()
    let resolveTypeShapeCached(t : Type) = dict.GetOrAdd(t, resolveTypeShape)

//////////////////////////////////
///////////// Section: Public API

[<AutoOpen>]
module TypeShapeModule =

    /// Computes the type shape for given type
    let getShape (t : Type) = TypeShapeImpl.resolveTypeShapeCached t
    /// Computes the type shape for given object
    let getObjectShape (obj : obj) = 
        if obj = null then raise <| new ArgumentNullException()
        TypeShapeImpl.resolveTypeShapeCached (obj.GetType())

    /// Computes the type shape for given type
    let shapeof<'T> = TypeShapeImpl.resolveTypeShapeCached typeof<'T>

    let private SomeU = Some()
    let inline private test0<'T> (t : TypeShape) =
        match t with
        | :? TypeShape<'T> -> SomeU
        | _ -> None

    let inline private test1<'If> (t : TypeShape) =
        match box t with
        | :? 'If as f -> Some f
        | _ -> None

    let (|ShapeBool|_|) t = test0<bool> t
    let (|ShapeByte|_|) t = test0<byte> t
    let (|ShapeSByte|_|) t = test0<sbyte> t
    let (|ShapeInt16|_|) t = test0<int16> t
    let (|ShapeInt32|_|) t = test0<int32> t
    let (|ShapeInt64|_|) t = test0<int64> t
    let (|ShapeUInt16|_|) t = test0<uint16> t
    let (|ShapeUInt32|_|) t = test0<uint32> t
    let (|ShapeUInt64|_|) t = test0<uint64> t
    let (|ShapeSingle|_|) t = test0<single> t
    let (|ShapeDouble|_|) t = test0<double> t
    let (|ShapeChar|_|) t = test0<char> t

    let (|ShapeString|_|) t = test0<string> t
    let (|ShapeGuid|_|) t = test0<Guid> t
    let (|ShapeDecimal|_|) t = test0<decimal> t
    let (|ShapeTimeSpan|_|) t = test0<TimeSpan> t
    let (|ShapeDateTime|_|) t = test0<DateTime> t
    let (|ShapeDateTimeOffset|_|) t = test0<DateTimeOffset> t
    let (|ShapeDBNull|_|) t = test0<DBNull> t
    let (|ShapeUnit|_|) t = test0<unit> t
    let (|ShapeFSharpUnit|_|) t = test0<unit> t
    let (|ShapeByteArray|_|) t = test0<byte []> t
    
    let (|ShapeNullable|_|) t = test1<IShapeNullable> t
    let (|ShapeEnum|_|) t = test1<IShapeEnum> t
    let (|ShapeKeyValuePair|_|) t = test1<IShapeKeyValuePair> t
    let (|ShapeDictionary|_|) t = test1<IShapeDictionary> t
    let (|ShapeHashSet|_|) t = test1<IShapeHashSet> t
    let (|ShapeResizeArray|_|) t = test1<IShapeResizeArray> t

    let (|ShapeArray|_|) t = test1<IShapeArray> t
    let (|ShapeArray2D|_|) t = test1<IShapeArray2D> t
    let (|ShapeArray3D|_|) t = test1<IShapeArray3D> t
    let (|ShapeArray4D|_|) t = test1<IShapeArray4D> t

    let (|ShapeTuple1|_|) t = test1<IShapeTuple1> t
    let (|ShapeTuple2|_|) t = test1<IShapeTuple2> t
    let (|ShapeTuple3|_|) t = test1<IShapeTuple3> t
    let (|ShapeTuple4|_|) t = test1<IShapeTuple4> t
    let (|ShapeTuple5|_|) t = test1<IShapeTuple5> t
    let (|ShapeTuple6|_|) t = test1<IShapeTuple6> t
    let (|ShapeTuple7|_|) t = test1<IShapeTuple7> t
    let (|ShapeTuple8|_|) t = test1<IShapeTuple8> t

    let (|ShapeFSharpList|_|) t = test1<IShapeFSharpList> t
    let (|ShapeFSharpOption|_|) t = test1<IShapeFSharpOption> t
    let (|ShapeFSharpRef|_|) t = test1<IShapeFSharpRef> t
    let (|ShapeFSharpSet|_|) t = test1<IShapeFSharpSet> t
    let (|ShapeFSharpMap|_|) t = test1<IShapeFSharpMap> t
    let (|ShapeFSharpFunc|_|) t = test1<IShapeFSharpFunc> t
    let (|ShapeFSharpException|_|) t = test1<IShapeException> t

    let (|ShapeFSharpUnion1|_|) t = test1<IShapeFSharpUnion1> t
    let (|ShapeFSharpUnion2|_|) t = test1<IShapeFSharpUnion2> t
    let (|ShapeFSharpUnion3|_|) t = test1<IShapeFSharpUnion3> t
    let (|ShapeFSharpUnion4|_|) t = test1<IShapeFSharpUnion4> t
    let (|ShapeFSharpUnion5|_|) t = test1<IShapeFSharpUnion5> t
    let (|ShapeFSharpUnion6|_|) t = test1<IShapeFSharpUnion6> t
    let (|ShapeFSharpUnion7|_|) t = test1<IShapeFSharpUnion7> t

    let (|ShapeFSharpRecord1|_|) t = test1<IShapeFSharpRecord1> t
    let (|ShapeFSharpRecord2|_|) t = test1<IShapeFSharpRecord2> t
    let (|ShapeFSharpRecord3|_|) t = test1<IShapeFSharpRecord3> t
    let (|ShapeFSharpRecord4|_|) t = test1<IShapeFSharpRecord4> t
    let (|ShapeFSharpRecord5|_|) t = test1<IShapeFSharpRecord5> t
    let (|ShapeFSharpRecord6|_|) t = test1<IShapeFSharpRecord6> t
    let (|ShapeFSharpRecord7|_|) t = test1<IShapeFSharpRecord7> t

    let (|ShapeCollection|_|) t = test1<IShapeCollection> t
    let (|ShapeEnumerable|_|) t = test1<IShapeEnumerable> t

    let (|ShapePrimitive|_|) (t : TypeShape) =
        match t with
        | :? TypeShape<bool>  
        | :? TypeShape<byte>  
        | :? TypeShape<sbyte> 
        | :? TypeShape<int16> 
        | :? TypeShape<int32> 
        | :? TypeShape<int64> 
        | :? TypeShape<uint16>
        | :? TypeShape<uint32>
        | :? TypeShape<uint64>
        | :? TypeShape<single>
        | :? TypeShape<double>
        | :? TypeShape<decimal> -> SomeU
        | _ -> None

    let (|ShapeTuple|_|) (t : TypeShape) =
        match box t with
        | :? IShapeTuple1
        | :? IShapeTuple2
        | :? IShapeTuple3
        | :? IShapeTuple4
        | :? IShapeTuple5
        | :? IShapeTuple6
        | :? IShapeTuple7
        | :? IShapeTuple8 -> SomeU
        | _ -> None