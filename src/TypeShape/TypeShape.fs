#if TYPESHAPE_EXPOSE
module TypeShape
#else
module internal TypeShape
#endif

#nowarn "4224"

open System
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Reflection

//////////////////////////////////////////////////
///////////// Section: TypeShape core definitions

/// Provides a simple breakdown of basic kinds of types.
/// Used for easier extraction of type shapes in the active pattern implementations.
[<NoEquality; NoComparison>]
type TypeShapeInfo =
    | Basic of Type
    | Enum of enumTy:Type * underlying:Type
    | Array of element:Type * rank:int
    | Generic of definition:Type * args:Type []

type ITypeShapeVisitor<'R> =
    abstract Visit<'T> : unit -> 'R

[<AbstractClass>]
type TypeShape =
    [<CompilerMessage("TypeShape constructor should not be consumed.", 4224)>]
    internal new () = { }
    abstract Type : Type
    abstract ShapeInfo : TypeShapeInfo
    abstract Accept : ITypeShapeVisitor<'R> -> 'R
    override s.ToString() = sprintf "TypeShape [%O]" s.Type

[<Sealed>]
type TypeShape<'T> () =
    inherit TypeShape()
    static let shapeInfo =
        let t = typeof<'T>
        if t.IsEnum then
            Enum(t, t.GetEnumUnderlyingType())
        elif t.IsArray then
            Array(t.GetElementType(), t.GetArrayRank())
        elif t.IsGenericType then 
            Generic(t.GetGenericTypeDefinition(), t.GetGenericArguments())
        else
            Basic t
        
    override __.Type = typeof<'T>
    override __.ShapeInfo = shapeInfo
    override __.Accept v = v.Visit<'T> ()

//////////////////////////////////////
///////////// Section: Core BCL types

///////////// Enum types

type IEnumVisitor<'R> =
    abstract Visit<'Enum, 'Underlying when 'Enum : enum<'Underlying>> : unit -> 'R

type IShapeEnum =
    abstract Accept : IEnumVisitor<'R> -> 'R

type private ShapeEnum<'Enum, 'Underlying when 'Enum : enum<'Underlying>>() =
    interface IShapeEnum with
        member __.Accept v = v.Visit<'Enum, 'Underlying> ()

///////////// Nullable types

type INullableVisitor<'R> =
    abstract Visit<'T when 'T : (new : unit -> 'T) and 'T :> ValueType and 'T : struct> : unit -> 'R

type IShapeNullable =
    abstract Accept : INullableVisitor<'R> -> 'R

type private ShapeNullable<'T when 'T : (new : unit -> 'T) and 'T :> ValueType and 'T : struct> () =
    interface IShapeNullable with
        member __.Accept v = v.Visit<'T> ()

///////////// Delegates

type IDelegateVisitor<'R> =
    abstract Visit<'Delegate when 'Delegate :> Delegate> : unit -> 'R

type IShapeDelegate =
    abstract Accept : IDelegateVisitor<'R> -> 'R

type private ShapeDelegate<'Delegate when 'Delegate :> Delegate>() =
    interface IShapeDelegate with
        member __.Accept v = v.Visit<'Delegate>()

///////////// System.Tuple

type ITuple1Visitor<'R> =
    abstract Visit<'T> : unit -> 'R

type IShapeTuple1 =
    abstract Accept : ITuple1Visitor<'R> -> 'R

type private ShapeTuple<'T> () =
    interface IShapeTuple1 with
        member __.Accept v = v.Visit<'T> ()

/////////////

type ITuple2Visitor<'R> =
    abstract Visit<'T1, 'T2> : unit -> 'R

type IShapeTuple2 =
    abstract Accept : ITuple2Visitor<'R> -> 'R

type private ShapeTuple<'T1, 'T2> () =
    interface IShapeTuple2 with
        member __.Accept v = v.Visit<'T1,'T2> ()

/////////////

type ITuple3Visitor<'R> =
    abstract Visit<'T1, 'T2, 'T3> : unit -> 'R

type IShapeTuple3 =
    abstract Accept : ITuple3Visitor<'R> -> 'R

type private ShapeTuple<'T1, 'T2, 'T3> () =
    interface IShapeTuple3 with
        member __.Accept v = v.Visit<'T1, 'T2, 'T3> ()

/////////////

type ITuple4Visitor<'R> =
    abstract Visit<'T1, 'T2, 'T3, 'T4> : unit -> 'R

type IShapeTuple4 =
    abstract Accept : ITuple4Visitor<'R> -> 'R

type private ShapeTuple<'T1, 'T2, 'T3, 'T4> () =
    interface IShapeTuple4 with
        member __.Accept v = v.Visit<'T1, 'T2, 'T3, 'T4> ()

/////////////

type ITuple5Visitor<'R> =
    abstract Visit<'T1, 'T2, 'T3, 'T4, 'T5> : unit -> 'R

type IShapeTuple5 =
    abstract Accept : ITuple5Visitor<'R> -> 'R

type private ShapeTuple<'T1, 'T2, 'T3, 'T4, 'T5> () =
    interface IShapeTuple5 with
        member __.Accept v = v.Visit<'T1, 'T2, 'T3, 'T4, 'T5> ()

/////////////

type ITuple6Visitor<'R> =
    abstract Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> : unit -> 'R

type IShapeTuple6 =
    abstract Accept : ITuple6Visitor<'R> -> 'R

type private ShapeTuple<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> () =
    interface IShapeTuple6 with
        member __.Accept v = v.Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> ()

/////////////

type ITuple7Visitor<'R> =
    abstract Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> : unit -> 'R

type IShapeTuple7 =
    abstract Accept : ITuple7Visitor<'R> -> 'R

type private ShapeTuple<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> () =
    interface IShapeTuple7 with
        member __.Accept v = v.Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> ()

/////////////

type ITuple8Visitor<'R> =
    abstract Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'TRest> : unit -> 'R

type IShapeTuple8 =
    abstract Accept : ITuple8Visitor<'R> -> 'R

type private ShapeTuple<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'TRest> () =
    interface IShapeTuple8 with
        member __.Accept v = v.Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'TRest> ()

/////////// F# functions

type IFSharpFuncVisitor<'R> =
    abstract Visit<'Domain, 'CoDomain> : unit -> 'R

type IShapeFSharpFunc =
    abstract Accept : IFSharpFuncVisitor<'R> -> 'R

type private ShapeFSharpFunc<'Domain, 'CoDomain> () =
    interface IShapeFSharpFunc with
        member __.Accept v = v.Visit<'Domain, 'CoDomain> ()

/////////// System.Exception

type IExceptionVisitor<'R> =
    abstract Visit<'exn when 'exn :> exn> : unit -> 'R

type IShapeException =
    abstract IsFSharpException : bool
    abstract Accept : IExceptionVisitor<'R> -> 'R

type private ShapeException<'exn when 'exn :> exn> (isFSharpExn : bool) =
    interface IShapeException with
        member __.IsFSharpException = isFSharpExn
        member __.Accept v = v.Visit<'exn> ()


/////////////////////////////////////////////////
///////////// Section: Collections & IEnumerable

///////////// IEnumerable

type IEnumerableVisitor<'R> =
    abstract Visit<'Enum, 'T when 'Enum :> seq<'T>> : unit -> 'R

type IShapeEnumerable =
    abstract Accept : IEnumerableVisitor<'R> -> 'R

type private ShapeEnumerable<'Enum, 'T when 'Enum :> seq<'T>> () =
    interface IShapeEnumerable with
        member __.Accept v = v.Visit<'Enum, 'T> ()

///////////// Collection

type ICollectionVisitor<'R> =
    abstract Visit<'Collection, 'T when 'Collection :> ICollection<'T>> : unit -> 'R

type IShapeCollection =
    abstract Accept : ICollectionVisitor<'R> -> 'R

type private ShapeCollection<'Collection, 'T when 'Collection :> ICollection<'T>> () =
    interface IShapeCollection with
        member __.Accept v = v.Visit<'Collection, 'T> ()

///////////// KeyValuePair

type IKeyValuePairVisitor<'R> =
    abstract Visit<'K, 'V> : unit -> 'R

type IShapeKeyValuePair =
    abstract Accept : IKeyValuePairVisitor<'R> -> 'R

type private ShapeKeyValuePair<'K,'V> () =
    interface IShapeKeyValuePair with
        member __.Accept v = v.Visit<'K, 'V> ()

///////////// System.Array

///////////// Array 1D

type IArrayVisitor<'R> =
    abstract Visit<'T> : unit -> 'R

type IShapeArray =
    abstract Accept : IArrayVisitor<'R> -> 'R

type private ShapeArray<'T>() =
    interface IShapeArray with
        member __.Accept v = v.Visit<'T> ()

    interface IShapeCollection with
        member __.Accept v = v.Visit<'T[], 'T> ()

    interface IShapeEnumerable with
        member __.Accept v = v.Visit<'T[], 'T> ()

///////////// Array 2D

type IArray2DVisitor<'R> =
    abstract Visit<'T> : unit -> 'R

type IShapeArray2D =
    abstract Accept : IArray2DVisitor<'R> -> 'R

type private ShapeArray2D<'T>() =
    interface IShapeArray2D with
        member __.Accept v = v.Visit<'T> ()

///////////// Array 3D

type IArray3DVisitor<'R> =
    abstract Visit<'T> : unit -> 'R

type IShapeArray3D =
    abstract Accept : IArray3DVisitor<'R> -> 'R

type private ShapeArray3D<'T>() =
    interface IShapeArray3D with
        member __.Accept v = v.Visit<'T> ()

///////////// Array 4D

type IArray4DVisitor<'R> =
    abstract Visit<'T> : unit -> 'R

type IShapeArray4D =
    abstract Accept : IArray4DVisitor<'R> -> 'R

type private ShapeArray4D<'T>() =
    interface IShapeArray4D with
        member __.Accept v = v.Visit<'T> ()

///////////// System.Collections.List

type IResizeArrayVisitor<'R> =
    abstract Visit<'T> : unit -> 'R

type IShapeResizeArray =
    abstract Accept : IResizeArrayVisitor<'R> -> 'R

type private ShapeResizeArray<'T> () =
    interface IShapeResizeArray with
        member __.Accept v = v.Visit<'T> ()


///////////// System.Collections.Dictionary

type IDictionaryVisitor<'R> =
    abstract Visit<'K, 'V when 'K : equality> : unit -> 'R

type IShapeDictionary =
    abstract Accept : IDictionaryVisitor<'R> -> 'R

type private ShapeDictionary<'K, 'V when 'K : equality> () =
    interface IShapeDictionary with
        member __.Accept v = v.Visit<'K, 'V> ()

///////////// System.Collections.HashSet

type IHashSetVisitor<'R> =
    abstract Visit<'T when 'T : equality> : unit -> 'R

type IShapeHashSet =
    abstract Accept : IHashSetVisitor<'R> -> 'R

type private ShapeHashSet<'T when 'T : equality> () =
    interface IShapeHashSet with
        member __.Accept v = v.Visit<'T> ()

///////////// F# Set

type IFSharpSetVisitor<'R> =
    abstract Visit<'T when 'T : comparison> : unit -> 'R

type IShapeFSharpSet =
    abstract Accept : IFSharpSetVisitor<'R> -> 'R

type private ShapeFSharpSet<'T when 'T : comparison> () =
    interface IShapeFSharpSet with
        member __.Accept v = v.Visit<'T> ()

///////////// F# Map

type IFSharpMapVisitor<'R> =
    abstract Visit<'K, 'V when 'K : comparison> : unit -> 'R

type IShapeFSharpMap =
    abstract Accept : IFSharpMapVisitor<'R> -> 'R

type private ShapeFSharpMap<'K, 'V when 'K : comparison> () =
    interface IShapeFSharpMap with
        member __.Accept v = v.Visit<'K, 'V>()

//////////////////////////////////
///////////// Section: F# Records

type private RecordInfo =
    {
        RProperties : PropertyInfo []
        RCtor : ConstructorInfo
    }

type IShapeFSharpRecord =
    abstract ConstructorInfo : ConstructorInfo
    abstract Properties : PropertyInfo list

type private ShapeFSharpRecord<'Record>(info : RecordInfo) =
    interface IShapeFSharpRecord with
        member __.ConstructorInfo = info.RCtor
        member __.Properties = info.RProperties |> Array.toList

///////////// Record arity 1

type IShapeFSharpRecord1 =
    inherit IShapeFSharpRecord
    abstract Accept<'R> : IFSharpRecord1Visitor<'R> -> 'R

and IShapeFSharpRecord<'Record, 'Field1> =
    inherit IShapeFSharpRecord1
    abstract Construct : 'Field1 -> 'Record
    abstract Project1 : 'Record -> 'Field1

and IFSharpRecord1Visitor<'R> =
    abstract Visit<'Record, 'Field1> : IShapeFSharpRecord<'Record, 'Field1> -> 'R

type private ShapeFSharpRecord<'Record, 'Field1> (info : RecordInfo) =
    interface IShapeFSharpRecord<'Record, 'Field1> with
        member __.ConstructorInfo = info.RCtor
        member __.Properties = info.RProperties |> Array.toList
        member __.Construct(f1 : 'Field1) = info.RCtor.Invoke [|f1|] :?> 'Record
        member __.Project1(record : 'Record) = info.RProperties.[0].GetValue(record, null) :?> 'Field1
        member s.Accept v = v.Visit<'Record, 'Field1> s

///////////// Record arity 2

type IShapeFSharpRecord2 =
    inherit IShapeFSharpRecord
    abstract Accept<'R> : IFSharpRecord2Visitor<'R> -> 'R

and IShapeFSharpRecord<'Record, 'Field1, 'Field2> =
    inherit IShapeFSharpRecord2
    abstract Construct : 'Field1 * 'Field2 -> 'Record
    abstract Project1 : 'Record -> 'Field1
    abstract Project2 : 'Record -> 'Field2

and IFSharpRecord2Visitor<'R> =
    abstract Visit<'Record, 'Field1, 'Field2> : IShapeFSharpRecord<'Record, 'Field1, 'Field2> -> 'R

type private ShapeFSharpRecord<'Record, 'Field1, 'Field2> (info : RecordInfo) =
    interface IShapeFSharpRecord<'Record, 'Field1, 'Field2> with
        member __.Properties = info.RProperties |> Array.toList
        member __.ConstructorInfo = info.RCtor
        member __.Construct(f1 : 'Field1, f2 : 'Field2) = info.RCtor.Invoke [|f1; f2|] :?> 'Record
        member __.Project1(record : 'Record) = info.RProperties.[0].GetValue(record, null) :?> 'Field1
        member __.Project2(record : 'Record) = info.RProperties.[1].GetValue(record, null) :?> 'Field2
        member s.Accept v = v.Visit<'Record, 'Field1, 'Field2> s

///////////// Record arity 3

type IShapeFSharpRecord3 =
    inherit IShapeFSharpRecord
    abstract Accept<'R> : IFSharpRecord3Visitor<'R> -> 'R

and IShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3> =
    inherit IShapeFSharpRecord3
    abstract Construct : 'Field1 * 'Field2 * 'Field3 -> 'Record
    abstract Project1 : 'Record -> 'Field1
    abstract Project2 : 'Record -> 'Field2
    abstract Project3 : 'Record -> 'Field3

and IFSharpRecord3Visitor<'R> =
    abstract Visit<'Record, 'Field1, 'Field2, 'Field3> : IShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3> -> 'R

type private ShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3> (info : RecordInfo) =
    interface IShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3> with
        member __.ConstructorInfo = info.RCtor
        member __.Properties = info.RProperties |> Array.toList
        member __.Construct(f1 : 'Field1, f2 : 'Field2, f3 : 'Field3) = info.RCtor.Invoke [|f1; f2; f3|] :?> 'Record
        member __.Project1(record : 'Record) = info.RProperties.[0].GetValue(record, null) :?> 'Field1
        member __.Project2(record : 'Record) = info.RProperties.[1].GetValue(record, null) :?> 'Field2
        member __.Project3(record : 'Record) = info.RProperties.[2].GetValue(record, null) :?> 'Field3
        member s.Accept v = v.Visit<'Record, 'Field1, 'Field2, 'Field3> s

///////////// Record arity 4

type IShapeFSharpRecord4 =
    inherit IShapeFSharpRecord
    abstract Accept<'R> : IFSharpRecord4Visitor<'R> -> 'R

and IShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4> =
    inherit IShapeFSharpRecord4
    abstract Construct : 'Field1 * 'Field2 * 'Field3 * 'Field4 -> 'Record
    abstract Project1 : 'Record -> 'Field1
    abstract Project2 : 'Record -> 'Field2
    abstract Project3 : 'Record -> 'Field3
    abstract Project4 : 'Record -> 'Field4

and IFSharpRecord4Visitor<'R> =
    abstract Visit<'Record, 'Field1, 'Field2, 'Field3, 'Field4> : IShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4> -> 'R

type private ShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4> (info : RecordInfo) =
    interface IShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4> with
        member __.ConstructorInfo = info.RCtor
        member __.Properties = info.RProperties |> Array.toList
        member __.Construct(f1 : 'Field1, f2 : 'Field2, f3 : 'Field3, f4 : 'Field4) = info.RCtor.Invoke [|f1; f2; f3; f4|] :?> 'Record
        member __.Project1(record : 'Record) = info.RProperties.[0].GetValue(record, null) :?> 'Field1
        member __.Project2(record : 'Record) = info.RProperties.[1].GetValue(record, null) :?> 'Field2
        member __.Project3(record : 'Record) = info.RProperties.[2].GetValue(record, null) :?> 'Field3
        member __.Project4(record : 'Record) = info.RProperties.[3].GetValue(record, null) :?> 'Field4
        member s.Accept v = v.Visit<'Record, 'Field1, 'Field2, 'Field3, 'Field4> s

///////////// Record arity 5

type IShapeFSharpRecord5 =
    inherit IShapeFSharpRecord
    abstract Accept<'R> : IFSharpRecord5Visitor<'R> -> 'R

and IFSharpRecord5Visitor<'R> =
    abstract Visit<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5> : IShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5> -> 'R

and IShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5> =
    inherit IShapeFSharpRecord5
    abstract Construct : 'Field1 * 'Field2 * 'Field3 * 'Field4 * 'Field5 -> 'Record
    abstract Project1 : 'Record -> 'Field1
    abstract Project2 : 'Record -> 'Field2
    abstract Project3 : 'Record -> 'Field3
    abstract Project4 : 'Record -> 'Field4
    abstract Project5 : 'Record -> 'Field5

type private ShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5> (info : RecordInfo) =
    interface IShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5> with
        member __.ConstructorInfo = info.RCtor
        member __.Properties = info.RProperties |> Array.toList
        member __.Construct(f1 : 'Field1, f2 : 'Field2, f3 : 'Field3, f4 : 'Field4, f5 : 'Field5) = info.RCtor.Invoke [|f1; f2; f3; f4; f5|] :?> 'Record
        member __.Project1(record : 'Record) = info.RProperties.[0].GetValue(record, null) :?> 'Field1
        member __.Project2(record : 'Record) = info.RProperties.[1].GetValue(record, null) :?> 'Field2
        member __.Project3(record : 'Record) = info.RProperties.[2].GetValue(record, null) :?> 'Field3
        member __.Project4(record : 'Record) = info.RProperties.[3].GetValue(record, null) :?> 'Field4
        member __.Project5(record : 'Record) = info.RProperties.[4].GetValue(record, null) :?> 'Field5
        member s.Accept v = v.Visit<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5> s

///////////// Record arity 6

type IShapeFSharpRecord6 =
    inherit IShapeFSharpRecord
    abstract Accept<'R> : IFSharpRecord6Visitor<'R> -> 'R

and IShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5, 'Field6> =
    inherit IShapeFSharpRecord6
    abstract Construct : 'Field1 * 'Field2 * 'Field3 * 'Field4 * 'Field5 * 'Field6 -> 'Record
    abstract Project1 : 'Record -> 'Field1
    abstract Project2 : 'Record -> 'Field2
    abstract Project3 : 'Record -> 'Field3
    abstract Project4 : 'Record -> 'Field4
    abstract Project5 : 'Record -> 'Field5
    abstract Project6 : 'Record -> 'Field6

and IFSharpRecord6Visitor<'R> =
    abstract Visit<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5, 'Field6> : IShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5, 'Field6> -> 'R

and private ShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5, 'Field6> (info : RecordInfo) =
    interface IShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5, 'Field6> with
        member __.ConstructorInfo = info.RCtor
        member __.Properties = info.RProperties |> Array.toList
        member __.Construct(f1 : 'Field1, f2 : 'Field2, f3 : 'Field3, f4 : 'Field4, f5 : 'Field5, f6 : 'Field6) = info.RCtor.Invoke [|f1; f2; f3; f4; f5 ; f6|] :?> 'Record
        member __.Project1(record : 'Record) = info.RProperties.[0].GetValue(record, null) :?> 'Field1
        member __.Project2(record : 'Record) = info.RProperties.[1].GetValue(record, null) :?> 'Field2
        member __.Project3(record : 'Record) = info.RProperties.[2].GetValue(record, null) :?> 'Field3
        member __.Project4(record : 'Record) = info.RProperties.[3].GetValue(record, null) :?> 'Field4
        member __.Project5(record : 'Record) = info.RProperties.[4].GetValue(record, null) :?> 'Field5
        member __.Project6(record : 'Record) = info.RProperties.[5].GetValue(record, null) :?> 'Field6
        member s.Accept v = v.Visit<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5, 'Field6> s

///////////// Record arity 7

type IShapeFSharpRecord7 =
    inherit IShapeFSharpRecord
    abstract Accept<'R> : IFSharpRecord7Visitor<'R> -> 'R

and IFSharpRecord7Visitor<'R> =
    abstract Visit<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5, 'Field6, 'Field67> : IShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5, 'Field6, 'Field67> -> 'R

and IShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5, 'Field6, 'Field7> =
    inherit IShapeFSharpRecord7
    abstract Construct : 'Field1 * 'Field2 * 'Field3 * 'Field4 * 'Field5 * 'Field6 * 'Field7 -> 'Record
    abstract Project1 : 'Record -> 'Field1
    abstract Project2 : 'Record -> 'Field2
    abstract Project3 : 'Record -> 'Field3
    abstract Project4 : 'Record -> 'Field4
    abstract Project5 : 'Record -> 'Field5
    abstract Project6 : 'Record -> 'Field6
    abstract Project7 : 'Record -> 'Field7

type private ShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5, 'Field6, 'Field7> private (info : RecordInfo) =
    interface IShapeFSharpRecord<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5, 'Field6, 'Field7> with
        member __.ConstructorInfo = info.RCtor
        member __.Properties = info.RProperties |> Array.toList
        member __.Construct(f1 : 'Field1, f2 : 'Field2, f3 : 'Field3, f4 : 'Field4, f5 : 'Field5, f6 : 'Field6, f7 : 'Field7) = 
            info.RCtor.Invoke [|f1; f2; f3; f4; f5; f6; f7|] :?> 'Record

        member __.Project1(record : 'Record) = info.RProperties.[0].GetValue(record, null) :?> 'Field1
        member __.Project2(record : 'Record) = info.RProperties.[1].GetValue(record, null) :?> 'Field2
        member __.Project3(record : 'Record) = info.RProperties.[2].GetValue(record, null) :?> 'Field3
        member __.Project4(record : 'Record) = info.RProperties.[3].GetValue(record, null) :?> 'Field4
        member __.Project5(record : 'Record) = info.RProperties.[4].GetValue(record, null) :?> 'Field5
        member __.Project6(record : 'Record) = info.RProperties.[5].GetValue(record, null) :?> 'Field6
        member __.Project7(record : 'Record) = info.RProperties.[6].GetValue(record, null) :?> 'Field7
        member s.Accept v = v.Visit<'Record, 'Field1, 'Field2, 'Field3, 'Field4, 'Field5, 'Field6, 'Field7> s

///////////// F# ref

type IShapeFSharpRef =
    abstract Accept : IFSharpRefVisitor<'R> -> 'R

and IFSharpRefVisitor<'R> =
    abstract Visit<'T> : unit -> 'R

type private ShapeFSharpRef<'T> () =
    interface IShapeFSharpRef with
        member __.Accept v = v.Visit<'T> ()

/////////////////////////////////
///////////// Section: F# Unions

type private CaseInfo = 
    {
        UnionCaseInfo : UnionCaseInfo
        PayloadType : Type
        UCtor : obj -> obj
        UProj : obj -> obj
    }

type private UnionInfo =
    {
        TagReader : obj -> int
        Cases : CaseInfo []
    }
with
    member __.UnionCaseInfo =
        __.Cases |> Seq.map (fun u -> u.UnionCaseInfo) |> Seq.toList

type IShapeFSharpUnion =
    abstract GetTagUntyped : obj -> int
    abstract UnionCaseInfo : UnionCaseInfo list

type private ShapeFSharpUnion<'Union> (info : UnionInfo) =
    interface IShapeFSharpUnion with
        member __.GetTagUntyped o = info.TagReader o
        member __.UnionCaseInfo = info.UnionCaseInfo

///////////// 1-case union

type IFSharpUnion1Visitor<'R> =
    abstract Visit<'Union, 'Case1> : IShapeFSharpUnion<'Union, 'Case1> -> 'R

and IShapeFSharpUnion1 =
    inherit IShapeFSharpUnion
    abstract Accept : IFSharpUnion1Visitor<'R> -> 'R

and IShapeFSharpUnion<'Union, 'Case1> =
    inherit IShapeFSharpUnion1
    abstract GetTag : 'Union -> int
    abstract Project : 'Union -> 'Case1
    abstract Construct1 : 'Case1 -> 'Union

type private ShapeFSharpUnion<'Union, 'Case1> (info : UnionInfo) =
    interface IShapeFSharpUnion<'Union, 'Case1> with
        member __.UnionCaseInfo = info.UnionCaseInfo
        member __.GetTag (u : 'Union) = info.TagReader (u :> _)
        member __.GetTagUntyped o = info.TagReader o

        member __.Project (u : 'Union) =
            let { UProj = proj } = info.Cases.[0]
            proj u :?> 'Case1

        member __.Construct1 (c : 'Case1) = 
            let { UCtor = ctor } = info.Cases.[0]
            ctor c :?> 'Union

        member self.Accept v = v.Visit<'Union, 'Case1> self

///////////// 2-case union

type IFSharpUnion2Visitor<'R> =
    abstract Visit<'Union, 'Case1, 'Case2> : IShapeFSharpUnion<'Union, 'Case1, 'Case2> -> 'R

and IShapeFSharpUnion2 =
    inherit IShapeFSharpUnion
    abstract Accept : IFSharpUnion2Visitor<'R> -> 'R

and IShapeFSharpUnion<'Union, 'Case1, 'Case2> =
    inherit IShapeFSharpUnion2
    abstract GetTag : 'Union -> int
    abstract Project : 'Union -> Choice<'Case1, 'Case2>
    abstract Construct1 : 'Case1 -> 'Union
    abstract Construct2 : 'Case2 -> 'Union

type private ShapeFSharpUnion<'Union, 'Case1, 'Case2> (info : UnionInfo) =
    interface IShapeFSharpUnion<'Union, 'Case1, 'Case2> with
        member __.UnionCaseInfo = info.UnionCaseInfo
        member __.GetTag (u : 'Union) = info.TagReader (u :> _)
        member __.GetTagUntyped o = info.TagReader o

        member __.Project (u : 'Union) =
            let tag = info.TagReader (u :> _)
            let { UProj = proj } = info.Cases.[tag]
            let value = proj u
            match tag with
            | 0 -> Choice1Of2(value :?> 'Case1)
            | _ -> Choice2Of2(value :?> 'Case2)

        member __.Construct1 (c : 'Case1) =
            let { UCtor = ctor } = info.Cases.[0]
            ctor c :?> 'Union

        member __.Construct2 (c : 'Case2) =
            let { UCtor = ctor } = info.Cases.[1]
            ctor c :?> 'Union

        member self.Accept(v : IFSharpUnion2Visitor<'R>) = v.Visit<'Union, 'Case1, 'Case2> self

///////////// 3-case union

type IFSharpUnion3Visitor<'R> =
    abstract Visit<'Union, 'Case1, 'Case2, 'Case3> : IShapeFSharpUnion<'Union, 'Case1, 'Case2, 'Case3> -> 'R

and IShapeFSharpUnion3 =
    inherit IShapeFSharpUnion
    abstract Accept : IFSharpUnion3Visitor<'R> -> 'R

and IShapeFSharpUnion<'Union, 'Case1, 'Case2, 'Case3> =
    inherit IShapeFSharpUnion3
    abstract GetTag : 'Union -> int
    abstract Project : 'Union -> Choice<'Case1, 'Case2, 'Case3>
    abstract Construct1 : 'Case1 -> 'Union
    abstract Construct2 : 'Case2 -> 'Union
    abstract Construct3 : 'Case3 -> 'Union

type private ShapeFSharpUnion<'Union, 'Case1, 'Case2, 'Case3> (info : UnionInfo) =
    interface IShapeFSharpUnion<'Union, 'Case1, 'Case2, 'Case3> with
        member __.UnionCaseInfo = info.UnionCaseInfo
        member __.GetTag (u : 'Union) = info.TagReader (u :> _)
        member __.GetTagUntyped o = info.TagReader o

        member __.Project (u : 'Union) =
            let tag = info.TagReader (u :> _)
            let { UProj = proj } = info.Cases.[tag]
            let value = proj u
            match tag with
            | 0 -> Choice1Of3(value :?> 'Case1)
            | 1 -> Choice2Of3(value :?> 'Case2)
            | _ -> Choice3Of3(value :?> 'Case3)

        member __.Construct1 (c : 'Case1) =
            let { UCtor = ctor } = info.Cases.[0]
            ctor c :?> 'Union

        member __.Construct2 (c : 'Case2) =
            let { UCtor = ctor } = info.Cases.[1]
            ctor c :?> 'Union

        member __.Construct3 (c : 'Case3) =
            let { UCtor = ctor } = info.Cases.[2]
            ctor c :?> 'Union

        member self.Accept(v : IFSharpUnion3Visitor<'R>) = 
            v.Visit<'Union, 'Case1, 'Case2, 'Case3> self

///////////// 4-case union

type IFSharpUnion4Visitor<'R> =
    abstract Visit<'Union, 'Case1, 'Case2, 'Case3, 'Case4> : IShapeFSharpUnion<'Union, 'Case1, 'Case2, 'Case3, 'Case4> -> 'R

and IShapeFSharpUnion4 =
    inherit IShapeFSharpUnion
    abstract Accept : IFSharpUnion4Visitor<'R> -> 'R

and IShapeFSharpUnion<'Union, 'Case1, 'Case2, 'Case3, 'Case4> =
    inherit IShapeFSharpUnion4
    abstract GetTag : 'Union -> int
    abstract Project : 'Union -> Choice<'Case1, 'Case2, 'Case3, 'Case4>
    abstract Construct1 : 'Case1 -> 'Union
    abstract Construct2 : 'Case2 -> 'Union
    abstract Construct3 : 'Case3 -> 'Union
    abstract Construct4 : 'Case4 -> 'Union

type private ShapeFSharpUnion<'Union, 'Case1, 'Case2, 'Case3, 'Case4> (info : UnionInfo) =
    interface IShapeFSharpUnion<'Union, 'Case1, 'Case2, 'Case3, 'Case4> with
        member __.UnionCaseInfo = info.UnionCaseInfo
        member __.GetTag (u : 'Union) = info.TagReader (u :> _)
        member __.GetTagUntyped o = info.TagReader o

        member __.Project (u : 'Union) =
            let tag = info.TagReader (u :> _)
            let { UProj = proj } = info.Cases.[tag]
            let value = proj u
            match tag with
            | 0 -> Choice1Of4(value :?> 'Case1)
            | 1 -> Choice2Of4(value :?> 'Case2)
            | 2 -> Choice3Of4(value :?> 'Case3)
            | _ -> Choice4Of4(value :?> 'Case4)

        member __.Construct1 (c : 'Case1) =
            let { UCtor = ctor } = info.Cases.[0]
            ctor c :?> 'Union

        member __.Construct2 (c : 'Case2) =
            let { UCtor = ctor } = info.Cases.[1]
            ctor c :?> 'Union

        member __.Construct3 (c : 'Case3) =
            let { UCtor = ctor } = info.Cases.[2]
            ctor c :?> 'Union

        member __.Construct4 (c : 'Case4) =
            let { UCtor = ctor } = info.Cases.[3]
            ctor c :?> 'Union

        member self.Accept(v : IFSharpUnion4Visitor<'R>) = 
            v.Visit<'Union, 'Case1, 'Case2, 'Case3, 'Case4> self

///////////// 5-case union

type IFSharpUnion5Visitor<'R> =
    abstract Visit<'Union, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5> : IShapeFSharpUnion<'Union, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5> -> 'R

and IShapeFSharpUnion5 =
    inherit IShapeFSharpUnion
    abstract Accept : IFSharpUnion5Visitor<'R> -> 'R

and IShapeFSharpUnion<'Union, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5> =
    inherit IShapeFSharpUnion5
    abstract GetTag : 'Union -> int
    abstract Project : 'Union -> Choice<'Case1, 'Case2, 'Case3, 'Case4, 'Case5>
    abstract Construct1 : 'Case1 -> 'Union
    abstract Construct2 : 'Case2 -> 'Union
    abstract Construct3 : 'Case3 -> 'Union
    abstract Construct4 : 'Case4 -> 'Union
    abstract Construct5 : 'Case5 -> 'Union

type private ShapeFSharpUnion<'Union, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5> (info : UnionInfo) =
    interface IShapeFSharpUnion<'Union, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5> with
        member __.UnionCaseInfo = info.UnionCaseInfo
        member __.GetTag (u : 'Union) = info.TagReader (u :> _)
        member __.GetTagUntyped o = info.TagReader o

        member __.Project (u : 'Union) =
            let tag = info.TagReader (u :> _)
            let { UProj = proj } = info.Cases.[tag]
            let value = proj u
            match tag with
            | 0 -> Choice1Of5(value :?> 'Case1)
            | 1 -> Choice2Of5(value :?> 'Case2)
            | 2 -> Choice3Of5(value :?> 'Case3)
            | 3 -> Choice4Of5(value :?> 'Case4)
            | _ -> Choice5Of5(value :?> 'Case5)

        member __.Construct1 (c : 'Case1) =
            let { UCtor = ctor } = info.Cases.[0]
            ctor c :?> 'Union

        member __.Construct2 (c : 'Case2) =
            let { UCtor = ctor } = info.Cases.[1]
            ctor c :?> 'Union

        member __.Construct3 (c : 'Case3) =
            let { UCtor = ctor } = info.Cases.[2]
            ctor c :?> 'Union

        member __.Construct4 (c : 'Case4) =
            let { UCtor = ctor } = info.Cases.[3]
            ctor c :?> 'Union

        member __.Construct5 (c : 'Case5) =
            let { UCtor = ctor } = info.Cases.[4]
            ctor c :?> 'Union

        member self.Accept(v : IFSharpUnion5Visitor<'R>) = 
            v.Visit<'Union, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5> self

///////////// 6-case union

type IFSharpUnion6Visitor<'R> =
    abstract Visit<'Union, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6> : IShapeFSharpUnion<'Union, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6> -> 'R

and IShapeFSharpUnion6 =
    inherit IShapeFSharpUnion
    abstract Accept : IFSharpUnion6Visitor<'R> -> 'R

and IShapeFSharpUnion<'Union, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6> =
    inherit IShapeFSharpUnion6
    abstract GetTag : 'Union -> int
    abstract Project : 'Union -> Choice<'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6>
    abstract Construct1 : 'Case1 -> 'Union
    abstract Construct2 : 'Case2 -> 'Union
    abstract Construct3 : 'Case3 -> 'Union
    abstract Construct4 : 'Case4 -> 'Union
    abstract Construct5 : 'Case5 -> 'Union
    abstract Construct6 : 'Case6 -> 'Union

type private ShapeFSharpUnion<'Union, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6> (info : UnionInfo) =
    interface IShapeFSharpUnion<'Union, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6> with
        member __.UnionCaseInfo = info.UnionCaseInfo
        member __.GetTag (u : 'Union) = info.TagReader (u :> _)
        member __.GetTagUntyped o = info.TagReader o

        member __.Project (u : 'Union) =
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

        member __.Construct1 (c : 'Case1) =
            let { UCtor = ctor } = info.Cases.[0]
            ctor c :?> 'Union

        member __.Construct2 (c : 'Case2) =
            let { UCtor = ctor } = info.Cases.[1]
            ctor c :?> 'Union

        member __.Construct3 (c : 'Case3) =
            let { UCtor = ctor } = info.Cases.[2]
            ctor c :?> 'Union

        member __.Construct4 (c : 'Case4) =
            let { UCtor = ctor } = info.Cases.[3]
            ctor c :?> 'Union

        member __.Construct5 (c : 'Case5) =
            let { UCtor = ctor } = info.Cases.[4]
            ctor c :?> 'Union

        member __.Construct6 (c : 'Case6) =
            let { UCtor = ctor } = info.Cases.[5]
            ctor c :?> 'Union

        member self.Accept(v : IFSharpUnion6Visitor<'R>) = 
            v.Visit<'Union, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6> self

///////////// 7-case union

type IFSharpUnion7Visitor<'R> =
    abstract Visit<'Union, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6, 'Case7> : IShapeFSharpUnion<'Union, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6, 'Case7> -> 'R

and IShapeFSharpUnion7 =
    inherit IShapeFSharpUnion
    abstract Accept : IFSharpUnion7Visitor<'R> -> 'R

and IShapeFSharpUnion<'Union, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6, 'Case7> =
    inherit IShapeFSharpUnion7
    abstract GetTag : 'Union -> int
    abstract Project : 'Union -> Choice<'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6, 'Case7>
    abstract Construct1 : 'Case1 -> 'Union
    abstract Construct2 : 'Case2 -> 'Union
    abstract Construct3 : 'Case3 -> 'Union
    abstract Construct4 : 'Case4 -> 'Union
    abstract Construct5 : 'Case5 -> 'Union
    abstract Construct6 : 'Case6 -> 'Union
    abstract Construct7 : 'Case7 -> 'Union


type private ShapeFSharpUnion<'Union, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6, 'Case7> (info : UnionInfo) =
    interface IShapeFSharpUnion<'Union, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6, 'Case7> with
        member __.UnionCaseInfo = info.UnionCaseInfo
        member __.GetTag (u : 'Union) = info.TagReader (u :> _)
        member __.GetTagUntyped o = info.TagReader o

        member __.Project (u : 'Union) =
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

        member __.Construct1 (c : 'Case1) =
            let { UCtor = ctor } = info.Cases.[0]
            ctor c :?> 'Union

        member __.Construct2 (c : 'Case2) =
            let { UCtor = ctor } = info.Cases.[1]
            ctor c :?> 'Union

        member __.Construct3 (c : 'Case3) =
            let { UCtor = ctor } = info.Cases.[2]
            ctor c :?> 'Union

        member __.Construct4 (c : 'Case4) =
            let { UCtor = ctor } = info.Cases.[3]
            ctor c :?> 'Union

        member __.Construct5 (c : 'Case5) =
            let { UCtor = ctor } = info.Cases.[4]
            ctor c :?> 'Union

        member __.Construct6 (c : 'Case6) =
            let { UCtor = ctor } = info.Cases.[5]
            ctor c :?> 'Union

        member __.Construct7 (c : 'Case7) =
            let { UCtor = ctor } = info.Cases.[6]
            ctor c :?> 'Union

        member self.Accept(v : IFSharpUnion7Visitor<'R>) = 
            v.Visit<'Union, 'Case1, 'Case2, 'Case3, 'Case4, 'Case5, 'Case6, 'Case7> self

///////////// F# option

type IFSharpOptionVisitor<'R> =
    abstract Visit<'T> : unit -> 'R

type IShapeFSharpOption =
    abstract Accept : IFSharpOptionVisitor<'R> -> 'R

type private ShapeFSharpOption<'T> () =
    interface IShapeFSharpOption with
        member __.Accept v = v.Visit<'T> ()

///////////// F# List

type IFSharpListVisitor<'R> =
    abstract Visit<'T> : unit -> 'R

type IShapeFSharpList =
    abstract Accept : IFSharpListVisitor<'R> -> 'R

type private ShapeFSharpList<'T> () =
    interface IShapeFSharpList with
        member __.Accept v = v.Visit<'T> ()

///////////// F# Choice 2

type IFSharpChoice2Visitor<'R> =
    abstract Visit<'T1,'T2> : unit -> 'R

type IShapeFSharpChoice2 =
    abstract Accept : IFSharpChoice2Visitor<'R> -> 'R

type private ShapeFSharpChoice<'T1, 'T2> () =
    interface IShapeFSharpChoice2 with
        member __.Accept v = v.Visit<'T1,'T2>()

///////////// F# Choice 3

type IFSharpChoice3Visitor<'R> =
    abstract Visit<'T1,'T2,'T3> : unit -> 'R

type IShapeFSharpChoice3 =
    abstract Accept : IFSharpChoice3Visitor<'R> -> 'R

type private ShapeFSharpChoice<'T1, 'T2, 'T3> () =
    interface IShapeFSharpChoice3 with
        member __.Accept v = v.Visit<'T1,'T2,'T3>()

///////////// F# Choice 4

type IFSharpChoice4Visitor<'R> =
    abstract Visit<'T1,'T2,'T3,'T4> : unit -> 'R

type IShapeFSharpChoice4 =
    abstract Accept : IFSharpChoice4Visitor<'R> -> 'R

type private ShapeFSharpChoice<'T1, 'T2, 'T3, 'T4> () =
    interface IShapeFSharpChoice4 with
        member __.Accept v = v.Visit<'T1,'T2,'T3,'T4>()

///////////// F# Choice 5

type IFSharpChoice5Visitor<'R> =
    abstract Visit<'T1,'T2,'T3,'T4,'T5> : unit -> 'R

type IShapeFSharpChoice5 =
    abstract Accept : IFSharpChoice5Visitor<'R> -> 'R

type private ShapeFSharpChoice<'T1, 'T2, 'T3, 'T4, 'T5> () =
    interface IShapeFSharpChoice5 with
        member __.Accept v = v.Visit<'T1,'T2,'T3,'T4,'T5>()

///////////// F# Choice 6

type IFSharpChoice6Visitor<'R> =
    abstract Visit<'T1,'T2,'T3,'T4,'T5,'T6> : unit -> 'R

type IShapeFSharpChoice6 =
    abstract Accept : IFSharpChoice6Visitor<'R> -> 'R

type private ShapeFSharpChoice<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> () =
    interface IShapeFSharpChoice6 with
        member __.Accept v = v.Visit<'T1,'T2,'T3,'T4,'T5,'T6>()

///////////// F# Choice 7

type IFSharpChoice7Visitor<'R> =
    abstract Visit<'T1,'T2,'T3,'T4,'T5,'T6,'T7> : unit -> 'R

type IShapeFSharpChoice7 =
    abstract Accept : IFSharpChoice7Visitor<'R> -> 'R

type private ShapeFSharpChoice<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> () =
    interface IShapeFSharpChoice7 with
        member __.Accept v = v.Visit<'T1,'T2,'T3,'T4,'T5,'T6,'T7>()

////////////////////////////////////////////
///////////// Section: TypeShape resolution

exception UnsupportedShape of Type:Type
 with
    override __.Message = sprintf "Unsupported TypeShape '%O'" __.Type

[<AutoOpen>]
module private TypeShapeImpl =

    let allMembers =
        BindingFlags.NonPublic ||| BindingFlags.Public |||
            BindingFlags.Instance ||| BindingFlags.Static |||
                BindingFlags.FlattenHierarchy

    let activateGeneric (templateTy:Type) (typeArgs : Type[]) (args:obj[]) =
        let templateTy =
            if typeArgs.Length = 0 then templateTy
            elif not templateTy.IsGenericType then invalidArg (string templateTy) "not generic."
            elif not templateTy.IsGenericTypeDefinition then
                templateTy.GetGenericTypeDefinition().MakeGenericType typeArgs
            else
                templateTy.MakeGenericType typeArgs

        let ctypes = args |> Array.map (fun o -> o.GetType())
        let ctor = templateTy.GetConstructor(allMembers, null, CallingConventions.Standard, ctypes, [||])
        ctor.Invoke args

    /// correctly resolves if type is assignable to interface
    let rec isInterfaceAssignableFrom (iface : Type) (ty : Type) =
        let proj (t : Type) = t.Assembly, t.Namespace, t.Name, t.MetadataToken
        if iface = ty then true
        elif ty.GetInterfaces() |> Array.exists(fun if0 -> proj if0 = proj iface) then true
        else
            match ty.BaseType with
            | null -> false
            | bt -> isInterfaceAssignableFrom iface bt

    let private canon = Type.GetType "System.__Canon"

    let resolveTypeShape(typ : Type) =
        if typ = null then raise <| UnsupportedShape typ
        if typ.IsGenericTypeDefinition then raise <| UnsupportedShape typ
        elif typ.IsGenericParameter then raise <| UnsupportedShape typ
        elif typ = canon then raise <| UnsupportedShape typ
        elif typ.IsByRef || typ.IsPointer then raise <| UnsupportedShape typ
        else activateGeneric typedefof<TypeShape<_>> [|typ|] [||] :?> TypeShape

    let extractRecordInfo (recordType : Type) =
        let ctor = FSharpValue.PreComputeRecordConstructorInfo(recordType, allMembers)
        let properties = FSharpType.GetRecordFields(recordType, allMembers)
        { RCtor = ctor ; RProperties = properties }
   
    let extractUnionInfo (unionType : Type) =
        let mkCaseInfo (uci : UnionCaseInfo) =
            let fields = uci.GetFields()
            let uctor = FSharpValue.PreComputeUnionConstructor(uci, allMembers)
            match fields with
            | [||] -> 
                { UnionCaseInfo = uci ; PayloadType = typeof<unit> ;
                    UCtor = (fun _ -> uctor [||]) ;
                    UProj = (fun _ -> () :> _) }

            | [|field|] -> 
                { UnionCaseInfo = uci ; PayloadType = field.PropertyType ;
                    UCtor = (fun v -> uctor [|v|]) ;
                    UProj = (fun u -> field.GetValue(u, null)) }
            | _ ->
                let tupleType = fields |> Array.map (fun f -> f.PropertyType) |> FSharpType.MakeTupleType
                let uReader = FSharpValue.PreComputeUnionReader(uci, allMembers)
                let tupleCtor = FSharpValue.PreComputeTupleConstructor tupleType
                let tupleReader = FSharpValue.PreComputeTupleReader tupleType
                { UnionCaseInfo = uci ; PayloadType = tupleType ;
                    UCtor = tupleReader >> uctor ;
                    UProj = uReader >> tupleCtor }

        let tagReader = FSharpValue.PreComputeUnionTagReader(unionType, allMembers)
        let ucis = FSharpType.GetUnionCases(unionType, allMembers)
        let caseInfo = ucis |> Array.map mkCaseInfo
        { TagReader = tagReader ; Cases = caseInfo }

//////////////////////////////////
///////////// Section: Public API

type TypeShape with
    /// <summary>
    ///     Creates a type shape instance for given type
    /// </summary>
    /// <param name="typ">System.Type to be resolved.</param>
    static member Create(typ : Type) = resolveTypeShape typ
    /// <summary>
    ///     Creates a type shape instance for given type
    /// </summary>
    static member Create<'T>() = new TypeShape<'T>()

/// Creates a type shape instance for given type
let shapeof<'T> = TypeShape.Create<'T>()

type Activator with
    /// Generic edition of the activator method which support type parameters and private types
    static member CreateInstanceGeneric<'Template>(?typeArgs : Type[], ?args:obj[]) : obj =
        let typeArgs = defaultArg typeArgs [||]
        let args = defaultArg args [||]
        activateGeneric typeof<'Template> typeArgs args

type Type with
    /// Correctly resolves if type is assignable to interface
    member iface.IsInterfaceAssignableFrom(ty : Type) : bool =
        isInterfaceAssignableFrom iface ty

/// TypeShape active recognizers
[<RequireQualifiedAccess>]
module Shape =

    let private SomeU = Some() // avoid allocating this all the time
    let inline private test<'T> (s : TypeShape) =
        match s with
        | :? TypeShape<'T> -> SomeU
        | _ -> None

    let (|Bool|_|) s = test<bool> s
    let (|Byte|_|) s = test<byte> s
    let (|SByte|_|) s = test<sbyte> s
    let (|Int16|_|) s = test<int16> s
    let (|Int32|_|) s = test<int32> s
    let (|Int64|_|) s = test<int64> s
    let (|IntPtr|_|) s = test<nativeint> s
    let (|UInt16|_|) s = test<uint16> s
    let (|UInt32|_|) s = test<uint32> s
    let (|UInt64|_|) s = test<uint64> s
    let (|UIntPtr|_|) s = test<unativeint> s
    let (|Single|_|) s = test<single> s
    let (|Double|_|) s = test<double> s
    let (|Char|_|) s = test<char> s

    let (|String|_|) s = test<string> s
    let (|Guid|_|) s = test<Guid> s
    let (|Decimal|_|) s = test<decimal> s
    let (|TimeSpan|_|) s = test<TimeSpan> s
    let (|DateTime|_|) s = test<DateTime> s
    let (|DateTimeOffset|_|) s = test<DateTimeOffset> s
    let (|Unit|_|) s = test<unit> s
    let (|FSharpUnit|_|) s = test<unit> s
    let (|ByteArray|_|) s = test<byte []> s
    
    let (|Nullable|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<Nullable<_>> ->
            Activator.CreateInstanceGeneric<ShapeNullable<_>>(ta) 
            :?> IShapeNullable
            |> Some

        | _ -> None
        
    let (|Enum|_|) (s : TypeShape) = 
        match s.ShapeInfo with
        | Enum(e,u) ->
            Activator.CreateInstanceGeneric<ShapeEnum<BindingFlags, int>>([|e;u|])
            :?> IShapeEnum 
            |> Some
        | _ -> None

    let (|KeyValuePair|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<KeyValuePair<_,_>> ->
            Activator.CreateInstanceGeneric<ShapeKeyValuePair<_,_>>(ta)
            :?> IShapeKeyValuePair
            |> Some
        | _ ->
            None

    let (|Dictionary|_|) (s : TypeShape) = 
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<Dictionary<_,_>> ->
            Activator.CreateInstanceGeneric<ShapeDictionary<_,_>>(ta)
            :?> IShapeDictionary
            |> Some
        | _ ->
            None

    let (|HashSet|_|) (s : TypeShape) = 
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<HashSet<_>> ->
            Activator.CreateInstanceGeneric<ShapeHashSet<_>>(ta)
            :?> IShapeHashSet
            |> Some
        | _ ->
            None

    let (|ResizeArray|_|) (s : TypeShape) = 
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<ResizeArray<_>> ->
            Activator.CreateInstanceGeneric<ShapeResizeArray<_>>(ta)
            :?> IShapeResizeArray
            |> Some
        | _ ->
            None

    let (|Delegate|_|) (s : TypeShape) =
        if typeof<System.Delegate>.IsAssignableFrom s.Type then
            Activator.CreateInstanceGeneric<ShapeDelegate<_>>([|s.Type|])
            :?> IShapeDelegate
            |> Some
        else
            None

    let (|Exception|_|) (s : TypeShape) =
        if typeof<System.Exception>.IsAssignableFrom s.Type then
            let isFSharpExn = FSharpType.IsExceptionRepresentation(s.Type, allMembers)
            Activator.CreateInstanceGeneric<ShapeException<_>>([|s.Type|], [|isFSharpExn|])
            :?> IShapeException
            |> Some
        else
            None

    let (|Array|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | TypeShapeInfo.Array(et,1) ->
            Activator.CreateInstanceGeneric<ShapeArray<_>>([|et|])
            :?> IShapeArray
            |> Some
        | _ ->
            None
            
    let (|Array2D|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | TypeShapeInfo.Array(et,2) ->
            Activator.CreateInstanceGeneric<ShapeArray2D<_>>([|et|])
            :?> IShapeArray2D
            |> Some
        | _ -> None

    let (|Array3D|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | TypeShapeInfo.Array(et,3) ->
            Activator.CreateInstanceGeneric<ShapeArray3D<_>>([|et|])
            :?> IShapeArray3D
            |> Some
        | _ -> None

    let (|Array4D|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | TypeShapeInfo.Array(et,4) ->
            Activator.CreateInstanceGeneric<ShapeArray4D<_>>([|et|])
            :?> IShapeArray4D
            |> Some
        | _ -> None

    let (|Tuple|_|) (s : TypeShape) =
        if FSharpType.IsTuple s.Type then SomeU else None

    let (|Tuple1|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<Tuple<_>> ->
            Activator.CreateInstanceGeneric<ShapeTuple<_>>(ta)
            :?> IShapeTuple1
            |> Some
        | _ -> None

    let (|Tuple2|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<_ * _> ->
            Activator.CreateInstanceGeneric<ShapeTuple<_,_>>(ta)
            :?> IShapeTuple2
            |> Some
        | _ -> None

    let (|Tuple3|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<_ * _ * _> ->
            Activator.CreateInstanceGeneric<ShapeTuple<_,_,_>>(ta)
            :?> IShapeTuple3
            |> Some
        | _ -> None
        
    let (|Tuple4|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<_ * _ * _ * _> ->
            Activator.CreateInstanceGeneric<ShapeTuple<_,_,_,_>>(ta)
            :?> IShapeTuple4
            |> Some
        | _ -> None

    let (|Tuple5|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<_ * _ * _ * _ * _> ->
            Activator.CreateInstanceGeneric<ShapeTuple<_,_,_,_,_>>(ta)
            :?> IShapeTuple5
            |> Some
        | _ -> None

    let (|Tuple6|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<_ * _ * _ * _ * _ * _> ->
            Activator.CreateInstanceGeneric<ShapeTuple<_,_,_,_,_,_>>(ta)
            :?> IShapeTuple6
            |> Some
        | _ -> None

    let (|Tuple7|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<_ * _ * _ * _ * _ * _ * _> ->
            Activator.CreateInstanceGeneric<ShapeTuple<_,_,_,_,_,_,_>>(ta)
            :?> IShapeTuple7
            |> Some
        | _ -> None

    let (|Tuple8|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<Tuple<_,_,_,_,_,_,_,_>> ->
            Activator.CreateInstanceGeneric<ShapeTuple<_,_,_,_,_,_,_,_>>(ta)
            :?> IShapeTuple8
            |> Some
        | _ -> None

    let (|FSharpList|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<_ list> ->
            Activator.CreateInstanceGeneric<ShapeFSharpList<_>>(ta)
            :?> IShapeFSharpList
            |> Some
        | _ -> None

    let (|FSharpOption|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<_ option> ->
            Activator.CreateInstanceGeneric<ShapeFSharpOption<_>>(ta)
            :?> IShapeFSharpOption
            |> Some
        | _ -> None

    let (|FSharpRef|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<_ ref> ->
            Activator.CreateInstanceGeneric<ShapeFSharpRef<_>>(ta)
            :?> IShapeFSharpRef
            |> Some
        | _ -> None

    let (|FSharpSet|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<Set<_>> ->
            Activator.CreateInstanceGeneric<ShapeFSharpSet<_>>(ta)
            :?> IShapeFSharpSet
            |> Some
        | _ -> None

    let (|FSharpChoice2|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<Choice<_,_>> ->
            Activator.CreateInstanceGeneric<ShapeFSharpChoice<_,_>>(ta)
            :?> IShapeFSharpChoice2
            |> Some
        | _ -> None

    let (|FSharpChoice3|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<Choice<_,_,_>> ->
            Activator.CreateInstanceGeneric<ShapeFSharpChoice<_,_,_>>(ta)
            :?> IShapeFSharpChoice3
            |> Some
        | _ -> None

    let (|FSharpChoice4|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<Choice<_,_,_,_>> ->
            Activator.CreateInstanceGeneric<ShapeFSharpChoice<_,_,_,_>>(ta)
            :?> IShapeFSharpChoice4
            |> Some
        | _ -> None

    let (|FSharpChoice5|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<Choice<_,_,_,_,_>> ->
            Activator.CreateInstanceGeneric<ShapeFSharpChoice<_,_,_,_,_>>(ta)
            :?> IShapeFSharpChoice5
            |> Some
        | _ -> None

    let (|FSharpChoice6|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<Choice<_,_,_,_,_,_>> ->
            Activator.CreateInstanceGeneric<ShapeFSharpChoice<_,_,_,_,_,_>>(ta)
            :?> IShapeFSharpChoice6
            |> Some
        | _ -> None

    let (|FSharpChoice7|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<Choice<_,_,_,_,_,_,_>> ->
            Activator.CreateInstanceGeneric<ShapeFSharpChoice<_,_,_,_,_,_,_>>(ta)
            :?> IShapeFSharpChoice7
            |> Some
        | _ -> None

    let (|FSharpMap|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<Map<_,_>> -> 
            Activator.CreateInstanceGeneric<ShapeFSharpMap<_,_>>(ta)
            :?> IShapeFSharpMap
            |> Some
        | _ -> None

    let (|FSharpFunc|_|) (s : TypeShape) =
        if FSharpType.IsFunction s.Type then
            let d,c = FSharpType.GetFunctionElements s.Type
            Activator.CreateInstanceGeneric<ShapeFSharpFunc<_,_>> [|d;c|]
            :?> IShapeFSharpFunc
            |> Some
        else None

    let (|FSharpUnion|_|) (s : TypeShape) =
        if FSharpType.IsUnion(s.Type, allMembers) then
            let info = extractUnionInfo s.Type
            match info.Cases with
            | [|c1|] -> activateGeneric typedefof<ShapeFSharpUnion<_,_>> [|s.Type ; c1.PayloadType|] [|info|]
            | [|c1;c2|] -> activateGeneric typedefof<ShapeFSharpUnion<_,_,_>> [|s.Type ; c1.PayloadType ; c2.PayloadType|] [|info|]
            | [|c1;c2;c3|] -> activateGeneric typedefof<ShapeFSharpUnion<_,_,_,_>> [|s.Type ; c1.PayloadType ; c2.PayloadType ; c3.PayloadType|] [|info|]
            | [|c1;c2;c3;c4|] -> activateGeneric typedefof<ShapeFSharpUnion<_,_,_,_,_>> [|s.Type ; c1.PayloadType ; c2.PayloadType ; c3.PayloadType ; c4.PayloadType|] [|info|]
            | [|c1;c2;c3;c4;c5|] -> activateGeneric typedefof<ShapeFSharpUnion<_,_,_,_,_,_>> [|s.Type ; c1.PayloadType ; c2.PayloadType ; c3.PayloadType ; c4.PayloadType ; c5.PayloadType|] [|info|]
            | [|c1;c2;c3;c4;c5;c6|] -> activateGeneric typedefof<ShapeFSharpUnion<_,_,_,_,_,_,_>> [|s.Type ; c1.PayloadType ; c2.PayloadType ; c3.PayloadType ; c4.PayloadType ; c5.PayloadType ; c6.PayloadType|] [|info|]
            | [|c1;c2;c3;c4;c5;c6;c7|] -> activateGeneric typedefof<ShapeFSharpUnion<_,_,_,_,_,_,_,_>> [|s.Type ; c1.PayloadType ; c2.PayloadType ; c3.PayloadType ; c4.PayloadType ; c5.PayloadType ; c6.PayloadType ; c7.PayloadType|] [|info|]
            | _ -> activateGeneric typedefof<ShapeFSharpUnion<_>> [|s.Type|] [|info|]
            :?> IShapeFSharpUnion
            |> Some
        else None

    let (|FSharpUnion1|_|) (s : TypeShape) =
        match s with FSharpUnion(:? IShapeFSharpUnion1 as u) -> Some u | _ -> None

    let (|FSharpUnion2|_|) (s : TypeShape) =
        match s with FSharpUnion(:? IShapeFSharpUnion2 as u) -> Some u | _ -> None

    let (|FSharpUnion3|_|) (s : TypeShape) =
        match s with FSharpUnion(:? IShapeFSharpUnion3 as u) -> Some u | _ -> None

    let (|FSharpUnion4|_|) (s : TypeShape) =
        match s with FSharpUnion(:? IShapeFSharpUnion4 as u) -> Some u | _ -> None

    let (|FSharpUnion5|_|) (s : TypeShape) =
        match s with FSharpUnion(:? IShapeFSharpUnion5 as u) -> Some u | _ -> None

    let (|FSharpUnion6|_|) (s : TypeShape) =
        match s with FSharpUnion(:? IShapeFSharpUnion6 as u) -> Some u | _ -> None

    let (|FSharpUnion7|_|) (s : TypeShape) =
        match s with FSharpUnion(:? IShapeFSharpUnion7 as u) -> Some u | _ -> None

    let (|FSharpRecord|_|) (s : TypeShape) = 
        if FSharpType.IsRecord(s.Type, allMembers) then
            let info = extractRecordInfo s.Type
            match info.RProperties with
            | [|p1|] -> activateGeneric typedefof<ShapeFSharpRecord<_,_>> [|s.Type;p1.PropertyType|] [|info|]
            | [|p1;p2|] -> activateGeneric typedefof<ShapeFSharpRecord<_,_,_>> [|s.Type;p1.PropertyType;p2.PropertyType|] [|info|]
            | [|p1;p2;p3|] -> activateGeneric typedefof<ShapeFSharpRecord<_,_,_,_>> [|s.Type;p1.PropertyType;p2.PropertyType;p3.PropertyType|] [|info|]
            | [|p1;p2;p3;p4|] -> activateGeneric typedefof<ShapeFSharpRecord<_,_,_,_,_>> [|s.Type;p1.PropertyType;p2.PropertyType;p3.PropertyType;p4.PropertyType|] [|info|]
            | [|p1;p2;p3;p4;p5|] -> activateGeneric typedefof<ShapeFSharpRecord<_,_,_,_,_,_>> [|s.Type;p1.PropertyType;p2.PropertyType;p3.PropertyType;p4.PropertyType;p5.PropertyType|] [|info|]
            | [|p1;p2;p3;p4;p5;p6|] -> activateGeneric typedefof<ShapeFSharpRecord<_,_,_,_,_,_,_>> [|s.Type;p1.PropertyType;p2.PropertyType;p3.PropertyType;p4.PropertyType;p5.PropertyType;p6.PropertyType|] [|info|]
            | [|p1;p2;p3;p4;p5;p6;p7|] -> activateGeneric typedefof<ShapeFSharpRecord<_,_,_,_,_,_,_,_>> [|s.Type;p1.PropertyType;p2.PropertyType;p3.PropertyType;p4.PropertyType;p5.PropertyType;p6.PropertyType;p7.PropertyType|] [|info|]
            | _ -> activateGeneric typedefof<ShapeFSharpRecord<_>> [|s.Type|] [|info|]
            :?> IShapeFSharpRecord
            |> Some
        else
            None

    let (|FSharpRecord1|_|) s = 
        match s with FSharpRecord(:? IShapeFSharpRecord1 as r) -> Some r | _ -> None

    let (|FSharpRecord2|_|) s = 
        match s with FSharpRecord(:? IShapeFSharpRecord2 as r) -> Some r | _ -> None

    let (|FSharpRecord3|_|) s = 
        match s with FSharpRecord(:? IShapeFSharpRecord3 as r) -> Some r | _ -> None

    let (|FSharpRecord4|_|) s = 
        match s with FSharpRecord(:? IShapeFSharpRecord4 as r) -> Some r | _ -> None

    let (|FSharpRecord5|_|) s = 
        match s with FSharpRecord(:? IShapeFSharpRecord5 as r) -> Some r | _ -> None

    let (|FSharpRecord6|_|) s = 
        match s with FSharpRecord(:? IShapeFSharpRecord6 as r) -> Some r | _ -> None

    let (|FSharpRecord7|_|) s = 
        match s with FSharpRecord(:? IShapeFSharpRecord7 as r) -> Some r | _ -> None

    let (|Collection|_|) (s : TypeShape) =
        match s.Type.GetInterface("ICollection`1") with
        | null ->
            match s.ShapeInfo with
            | Generic(td,ta) when td = typedefof<ICollection<_>> ->
                Activator.CreateInstanceGeneric<ShapeCollection<_,_>> [|s.Type; ta.[0]|]
                :?> IShapeCollection
                |> Some
            | _ -> None
        | iface ->
            let args = iface.GetGenericArguments()
            Activator.CreateInstanceGeneric<ShapeCollection<_,_>> [|s.Type; args.[0]|]
            :?> IShapeCollection
            |> Some

    let (|Enumerable|_|) (s : TypeShape) =
        match s.Type.GetInterface("IEnumerable`1") with
        | null ->
            match s.ShapeInfo with
            | Generic(td,ta) when td = typedefof<IEnumerable<_>> ->
                Activator.CreateInstanceGeneric<ShapeEnumerable<_,_>> [|s.Type; ta.[0]|]
                :?> IShapeEnumerable
                |> Some
            | _ -> None
        | iface ->
            let args = iface.GetGenericArguments()
            Activator.CreateInstanceGeneric<ShapeEnumerable<_,_>> [|s.Type; args.[0]|]
            :?> IShapeEnumerable
            |> Some