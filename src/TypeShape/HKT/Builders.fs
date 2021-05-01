namespace TypeShape.HKT

open System
open System.Collections.Generic
open TypeShape.Core

// Builder interfaces: provides a collection of small interfaces used for
// building generic programs for different kinds of .NET & F# types.
// Interfaces can be combined on demand to author generic programs
// that supports arbitrary subsets of types.

//------------------------------------------
// Primitive Types

type IBoolBuilder<'F> =
    abstract Bool : unit -> App<'F, bool>

type IByteBuilder<'F> =
    abstract Byte : unit -> App<'F, byte>

type ISByteBuilder<'F> =
    abstract SByte : unit -> App<'F, sbyte>

type ICharBuilder<'F> =
    abstract Char : unit -> App<'F, char>

//-------------------------------------------

type IInt16Builder<'F> =
    abstract Int16 : unit -> App<'F, int16>

type IInt32Builder<'F> =
    abstract Int32 : unit -> App<'F, int>

type IInt64Builder<'F> =
    abstract Int64 : unit -> App<'F, int64>

//-------------------------------------------

type IUInt16Builder<'F> =
    abstract UInt16 : unit -> App<'F, uint16>

type IUInt32Builder<'F> =
    abstract UInt32 : unit -> App<'F, uint32>

type IUInt64Builder<'F> =
    abstract UInt64 : unit -> App<'F, uint64>

//-------------------------------------------

type ISingleBuilder<'F> =
    abstract Single : unit -> App<'F, single>

type IDoubleBuilder<'F> =
    abstract Double : unit -> App<'F, double>

type IDecimalBuilder<'F> =
    abstract Decimal : unit -> App<'F, decimal>

type IBigIntBuilder<'F> =
    abstract BigInt : unit -> App<'F, bigint>

//-------------------------------------------
// .NET Ground types

type IUnitBuilder<'F> =
    abstract Unit : unit -> App<'F, unit>

type IStringBuilder<'F> =
    abstract String : unit -> App<'F, string>

type IGuidBuilder<'F> =
    abstract Guid : unit -> App<'F, Guid>

type ITimeSpanBuilder<'F> =
    abstract TimeSpan : unit -> App<'F, TimeSpan>

type IDateTimeBuilder<'F> =
    abstract DateTime : unit -> App<'F, DateTime>

type IDateTimeOffsetBuilder<'F> =
    abstract DateTimeOffset : unit -> App<'F, DateTimeOffset>

//-------------------------------------------
// .NET Generic types

type INullableBuilder<'F> =
    abstract Nullable : App<'F, 't> -> App<'F, Nullable<'t>>

type IEnumBuilder<'F> =
    abstract Enum : App<'F, 'u> -> App<'F, 'e>
        when 'e : enum<'u>
        and 'e : struct
        and 'e :> ValueType
        and 'e : (new : unit -> 'e)

type IArrayBuilder<'F> =
    abstract Array : App<'F, 't> -> App<'F, 't []>

type IArray2DBuilder<'F> =
    abstract Array2D : App<'F, 't> -> App<'F, 't[,]>

type IArray3DBuilder<'F> =
    abstract Array3D : App<'F, 't> -> App<'F, 't[,,]>

type IArray4DBuilder<'F> =
    abstract Array4D : App<'F, 't> -> App<'F, 't[,,,]>

type IDictionaryBuilder<'F> =
    abstract Dictionary : App<'F, 'k> -> App<'F, 'v> -> App<'F, Dictionary<'k, 'v>>

type IKeyValuePairBuilder<'F> =
    abstract KeyValuePair : App<'F, 'k> -> App<'F, 'v> -> App<'F, KeyValuePair<'k, 'v>>

//-------------------------------------------
// F# Generic Collections

type IFSharpOptionBuilder<'F> =
    abstract Option : App<'F, 't> -> App<'F, 't option>

type IFSharpRefBuilder<'F> =
    abstract Ref : App<'F, 't> -> App<'F, 't ref>

type ITuple2Builder<'F> =
    abstract Tuple2 : App<'F, 't> -> App<'F, 's> -> App<'F, 't * 's>

type IFSharpListBuilder<'F> =
    abstract List : App<'F, 't> -> App<'F, 't list>

type IFSharpSetBuilder<'F> =
    abstract Set : App<'F, 't> -> App<'F, Set<'t>>

type IFSharpMapBuilder<'F> =
    abstract Map : App<'F, 'k> -> App<'F, 'v> -> App<'F, Map<'k, 'v>>

type IFSharpFuncBuilder<'F> =
    abstract Func : App<'F, 'a> -> App<'F, 'b> -> App<'F, 'a -> 'b>

//-------------------------------------------
// Algebraic datatypes

/// Helper interface used for extracting generic programs from fields of record types
/// Given a type 't and a field of type 'f, the interface converts a HKT instance of
/// type App<'F, 'field> into an instance of App<'G, 't>. The return type hides type information
/// for the field, but it might use a different HKT representation.
type IFieldExtractor<'F, 'G> =
    abstract Field<'t, 'field> : ShapeMember<'t, 'field> -> App<'F, 'field> -> App<'G, 't>

type ITupleBuilder<'F, 'G> =
    inherit IFieldExtractor<'F, 'G>
    /// Constructs a generic tuple HKT instance from shape metadata and extracted field instances
    abstract Tuple : ShapeTuple<'t> -> fields : App<'G, 't> [] -> App<'F, 't>

type IFSharpRecordBuilder<'F, 'G> =
    inherit IFieldExtractor<'F, 'G>
    /// Constructs a generic F# record HKT instance from shape metadata and extracted field instances
    abstract Record : ShapeFSharpRecord<'t> -> fields : App<'G, 't> [] -> App<'F, 't>

type IFSharpUnionBuilder<'F, 'G> =
    inherit IFieldExtractor<'F, 'G>
    /// Constructs a generic union HKT instance from shape metadata and extracted field instances
    abstract Union : ShapeFSharpUnion<'t> -> fieldss : App<'G, 't> [][] -> App<'F, 't>

type ICliMutableBuilder<'F, 'G> =
    inherit IFieldExtractor<'F, 'G>
    /// Constructs a generic CliMutable HKT instance from shape metadata and extracted field instances
    abstract CliMutable : ShapeCliMutable<'t> -> fields : App<'G, 't> [] -> App<'F, 't>

type IPocoBuilder<'F, 'G> =
    inherit IFieldExtractor<'F, 'G>
    /// Constructs a generic Poco HKT instance from shape metadata and extracted field instances
    abstract Poco : ShapePoco<'t> -> fields : App<'G, 't> [] -> App<'F, 't>

//---------------------------------

/// Composite Builder containig methods for common F# algebraic data types
type IFSharpTypeBuilder<'F, 'G> =
    inherit IBoolBuilder<'F>
    inherit IByteBuilder<'F>
    inherit ISByteBuilder<'F>
    inherit ICharBuilder<'F>

    inherit IInt16Builder<'F>
    inherit IInt32Builder<'F>
    inherit IInt64Builder<'F>

    inherit IUInt16Builder<'F>
    inherit IUInt32Builder<'F>
    inherit IUInt64Builder<'F>

    inherit ISingleBuilder<'F>
    inherit IDoubleBuilder<'F>
    inherit IDecimalBuilder<'F>
    inherit IBigIntBuilder<'F>

    inherit IUnitBuilder<'F>
    inherit IStringBuilder<'F>
    inherit IGuidBuilder<'F>

    inherit ITimeSpanBuilder<'F>
    inherit IDateTimeBuilder<'F>
    inherit IDateTimeOffsetBuilder<'F>

    inherit INullableBuilder<'F>
    inherit IEnumBuilder<'F>
    inherit IArrayBuilder<'F>

    inherit IFSharpOptionBuilder<'F>
    inherit IFSharpRefBuilder<'F>
    inherit IFSharpListBuilder<'F>
    inherit IFSharpMapBuilder<'F>
    inherit IFSharpSetBuilder<'F>

    inherit ITupleBuilder<'F, 'G>
    inherit IFSharpRecordBuilder<'F, 'G>
    inherit IFSharpUnionBuilder<'F, 'G>