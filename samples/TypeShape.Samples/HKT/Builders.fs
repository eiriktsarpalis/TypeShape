namespace TypeShape.HKT

open System
open TypeShape.Core

// Generic Program builder interfaces

type IUnitBuilder<'F when 'F :> HKT> =
    abstract Unit : unit -> App<'F, unit>

type IBoolBuilder<'F when 'F :> HKT> =
    abstract Bool : unit -> App<'F, bool>

type IInt32Builder<'F when 'F :> HKT> =
    abstract Int32 : unit -> App<'F, int>

type IInt64Builder<'F when 'F :> HKT> =
    abstract Int64 : unit -> App<'F, int64>

type IStringBuilder<'F when 'F :> HKT> =
    abstract String : unit -> App<'F, string>

type IGuidBuilder<'F when 'F :> HKT> =
    abstract Guid : unit -> App<'F, Guid>

type ITimeSpanBuilder<'F when 'F :> HKT> =
    abstract TimeSpan : unit -> App<'F, TimeSpan>

type IDateTimeBuilder<'F when 'F :> HKT> =
    abstract DateTime : unit -> App<'F, DateTime>

type IDateTimeOffsetBuilder<'F when 'F :> HKT> =
    abstract DateTimeOffset : unit -> App<'F, DateTimeOffset>

// Generic types

type IFSharpOptionBuilder<'F when 'F :> HKT> =
    abstract Option : App<'F, 't> -> App<'F, 't option>

type IFSharpListBuilder<'F when 'F :> HKT> =
    abstract List : App<'F, 't> -> App<'F, 't list>

type IArrayBuilder<'F when 'F :> HKT> =
    abstract Array : App<'F, 't> -> App<'F, 't []>

type IFSharpSetBuilder<'F when 'F :> HKT> =
    abstract Set : App<'F, 't> -> App<'F, Set<'t>>

type IFSharpMapBuilder<'F when 'F :> HKT> =
    abstract Map : App<'F, 'k> -> App<'F, 'm> -> App<'F, Map<'k, 'm>>

// Tuples, Records & Unions

type IFieldExtractor<'F, 'G when 'F :> HKT and 'G :> HKT> =
    abstract Field<'t, 'field> : ShapeMember<'t, 'field> -> App<'F, 'field> -> App<'G, 't>

type ITupleBuilder<'F, 'G when 'F :> HKT and 'G :> HKT> =
    inherit IFieldExtractor<'F, 'G>
    abstract Tuple : ShapeTuple<'t> -> fields : App<'G, 't> [] -> App<'F, 't>

type IFSharpRecordBuilder<'F, 'G when 'F :> HKT and 'G :> HKT> =
    inherit IFieldExtractor<'F, 'G>
    abstract Record : ShapeFSharpRecord<'t> -> fields : App<'G, 't> [] -> App<'F, 't>

type IFSharpUnionBuilder<'F, 'G when 'F :> HKT and 'G :> HKT> =
    inherit IFieldExtractor<'F, 'G>
    abstract Union : ShapeFSharpUnion<'t> -> fields : App<'G, 't> [][] -> App<'F, 't>

type IResolver<'F when 'F :> HKT> =
    abstract Resolve<'t> : unit -> App<'F, 't>

//----------------------------------------------------------------
// Collection of functions that call into builder methods using TS

module Fold =

    let private unwrap (x : App<'F,_> ) : App<'F,_> = unbox x

    let (|Unit|_|) (builder : IUnitBuilder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.Unit -> builder.Unit () |> unwrap |> Some
        | _ -> None

    let (|Bool|_|) (builder : IBoolBuilder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.Bool -> builder.Bool () |> unwrap |> Some
        | _ -> None

    let (|Int32|_|) (builder : IInt32Builder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.Int32 -> builder.Int32 () |> unwrap |> Some
        | _ -> None

    let (|Int64|_|) (builder : IInt32Builder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.Int64 -> builder.Int32 () |> unwrap |> Some
        | _ -> None

    let (|String|_|) (builder : IStringBuilder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.String -> builder.String () |> unwrap |> Some
        | _ -> None

    let (|Guid|_|) (builder : IGuidBuilder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.Guid -> builder.Guid () |> unwrap |> Some
        | _ -> None

    let (|TimeSpan|_|) (builder : ITimeSpanBuilder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.TimeSpan -> builder.TimeSpan () |> unwrap |> Some
        | _ -> None

    let (|DateTime|_|) (builder : IDateTimeBuilder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.DateTime -> builder.DateTime () |> unwrap |> Some
        | _ -> None

    let (|DateTimeOffset|_|) (builder : IDateTimeOffsetBuilder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.DateTimeOffset -> builder.DateTimeOffset () |> unwrap |> Some
        | _ -> None

    // Generic Types

    let (|Array|_|) (builder : IArrayBuilder<'F>) (self : IResolver<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.Array s when s.Rank = 1 ->
            s.Element.Accept { new ITypeVisitor<App<'F, 't> option> with
                member __.Visit<'e> () =
                    let rt = self.Resolve<'e> ()
                    builder.Array rt |> unwrap |> Some
            }

        | _ -> None

    let (|FSharpOption|_|) (builder : IFSharpOptionBuilder<'F>) (self : IResolver<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.FSharpOption s ->
            s.Element.Accept {
                new ITypeVisitor<App<'F, 't> option> with
                    member __.Visit<'e> () =
                        let rt = self.Resolve<'e> ()
                        builder.Option rt |> unwrap |> Some
            }

        | _ -> None

    let (|FSharpList|_|) (builder : IFSharpListBuilder<'F>) (self : IResolver<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.FSharpList s ->
            s.Element.Accept { new ITypeVisitor<App<'F, 't> option> with
                member __.Visit<'e>() =
                    let elem = self.Resolve<'e>()
                    builder.List elem |> unwrap |> Some }
        | _ -> None

    let (|FSharpSet|_|) (builder : IFSharpSetBuilder<'F>) (self : IResolver<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.FSharpSet s ->
            s.Accept { new IFSharpSetVisitor<App<'F, 't> option> with
                member __.Visit<'e when 'e : comparison>() =
                    let elem = self.Resolve<'e>()
                    builder.Set elem |> unwrap |> Some 
            }
        | _ -> None

    let (|FSharpMap|_|) (builder : IFSharpMapBuilder<'F>) (self : IResolver<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.FSharpMap s ->
            s.Accept { new IFSharpMapVisitor<App<'F, 't> option> with
                member __.Visit<'k, 'v when 'k : comparison>() =
                    let k = self.Resolve<'k>()
                    let v = self.Resolve<'v>()
                    builder.Map k v |> unwrap |> Some 
            }
        | _ -> None

    // Tuples, Records and Unions

    let private handleField (self : IResolver<'F>) (fieldBuilder : IFieldExtractor<'F, 'G>) (mem : IShapeMember<'t>) =
        mem.Accept {
            new IMemberVisitor<'t, App<'G, 't>> with
                member __.Visit(m : ShapeMember<'t, 'f>) =
                    let inner = self.Resolve<'f> ()
                    fieldBuilder.Field m inner
        }

    let (|Tuple|_|) (builder : ITupleBuilder<'F, 'G>) (self : IResolver<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.Tuple (:? ShapeTuple<'t> as s) ->
            let fields = s.Elements |> Array.map (handleField self builder)
            builder.Tuple s fields |> Some

        | _ -> None

    let (|FSharpRecord|_|) (builder : IFSharpRecordBuilder<'F, 'G>) (self : IResolver<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.FSharpRecord (:? ShapeFSharpRecord<'t> as s) ->
            let fields = s.Fields |> Array.map (handleField self builder)
            builder.Record s fields |> Some

        | _ -> None

    let (|FSharpUnion|_|) (builder : IFSharpUnionBuilder<'F, 'G>) (self : IResolver<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.FSharpUnion (:? ShapeFSharpUnion<'t> as s) ->
            let extractCaseFields (c : ShapeFSharpUnionCase<'t>) = 
                c.Fields |> Array.map (handleField self builder)

            let fieldss = s.UnionCases |> Array.map extractCaseFields
            builder.Union s fieldss |> Some

        | _ -> None