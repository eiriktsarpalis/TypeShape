namespace TypeShape.HKT

open System
open TypeShape.Core
open TypeShape.Core.Utils

/// Generic program folding context
type IFoldContext<'F> =
    /// Fold an instance for supplied type, given a `self` instance for resolving type dependencies
    abstract Fold<'t> : self:IGenericProgram<'F> -> App<'F, 't>
    
    inherit IDelayBuilder<'F>

/// Generic program interface implementation
and IGenericProgram<'F> =
    abstract Resolve<'t> : unit -> App<'F, 't>

and IDelayBuilder<'F> =
    /// Bootstrap generic programs for recursive types
    abstract Delay : Cell<App<'F, 't>> -> App<'F, 't>


[<RequireQualifiedAccess>]
module FoldContext =

    let private cache = new TypeCache()

    /// Builds a generic program using supplied folding context.
    /// Folding is performed recursively and all intermediate results are cached.
    let fold<'F, 't> (folder : IFoldContext<'F>) : App<'F, 't> =
        let mutable f = Unchecked.defaultof<App<'F, 't>>
        if cache.TryGetValue(&f) then f
        else
            use ctx = cache.CreateGenerationContext()
            let rec self =
                { new IGenericProgram<'F> with 
                    member __.Resolve<'a>() = 
                        match ctx.InitOrGetCachedValue<App<'F, 'a>> (folder.Delay) with
                        | Cached(value = f) -> f
                        | NotCached t ->
                            let f = folder.Fold<'a> self
                            ctx.Commit t f }

            self.Resolve()


/// TypeShape-driven generic program folding 
module Fold =
    
    let private unwrap (x : App<'F,_> ) : App<'F,_> = unbox x

    //------------------------------------
    // Primitives
    
    let (|Bool|_|) (builder : IBoolBuilder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.Bool -> builder.Bool () |> unwrap |> Some
        | _ -> None
    
    let (|Byte|_|) (builder : IByteBuilder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.Byte -> builder.Byte () |> unwrap |> Some
        | _ -> None
    
    let (|SByte|_|) (builder : ISByteBuilder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.SByte -> builder.SByte () |> unwrap |> Some
        | _ -> None

    //------------------------------------
    
    let (|Int16|_|) (builder : IInt16Builder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.Int16 -> builder.Int16 () |> unwrap |> Some
        | _ -> None
    
    let (|Int32|_|) (builder : IInt32Builder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.Int32 -> builder.Int32 () |> unwrap |> Some
        | _ -> None
    
    let (|Int64|_|) (builder : IInt64Builder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.Int64 -> builder.Int64 () |> unwrap |> Some
        | _ -> None

    //------------------------------------
    
    let (|UInt16|_|) (builder : IUInt16Builder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.UInt16 -> builder.UInt16 () |> unwrap |> Some
        | _ -> None
    
    let (|UInt32|_|) (builder : IUInt32Builder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.UInt32 -> builder.UInt32 () |> unwrap |> Some
        | _ -> None
    
    let (|UInt64|_|) (builder : IUInt64Builder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.UInt64 -> builder.UInt64 () |> unwrap |> Some
        | _ -> None

    //------------------------------------
    
    let (|Single|_|) (builder : ISingleBuilder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.Single -> builder.Single () |> unwrap |> Some
        | _ -> None
    
    let (|Double|_|) (builder : IDoubleBuilder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.Double -> builder.Double () |> unwrap |> Some
        | _ -> None
    
    let (|Decimal|_|) (builder : IDecimalBuilder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.Decimal -> builder.Decimal () |> unwrap |> Some
        | _ -> None
    
    let (|BigInt|_|) (builder : IBigIntBuilder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.BigInt -> builder.BigInt () |> unwrap |> Some
        | _ -> None
    
    //------------------------------------
    // Ground .NET types
    
    let (|Unit|_|) (builder : IUnitBuilder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.Unit -> builder.Unit () |> unwrap |> Some
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
    
    //------------------------------------
    // Generic .NET Types
    
    let (|Array|_|) (builder : IArrayBuilder<'F>) (ctx : IGenericProgram<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.Array s when s.Rank = 1 ->
            s.Element.Accept { new ITypeVisitor<App<'F, 't> option> with
                member __.Visit<'e> () =
                    let rt = ctx.Resolve<'e> ()
                    builder.Array rt |> unwrap |> Some
            }
    
        | _ -> None

    let (|Nullable|_|) (builder : INullableBuilder<'F>) (self : IGenericProgram<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.Nullable s ->
            s.Accept { new INullableVisitor<App<'F, 't> option> with
                member __.Visit<'e when 'e : struct and 'e :> ValueType and 'e : (new : unit -> 'e)>() =
                    let e = self.Resolve<'e>()
                    builder.Nullable e |> unwrap |> Some
            }
        | _ -> None

    let (|Enum|_|) (builder : IEnumBuilder<'F>) (self : IGenericProgram<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.Enum s ->
            s.Accept { new IEnumVisitor<App<'F, 't> option> with
                member __.Visit<'e, 'u when 'e : enum<'u>
                                        and 'e : struct
                                        and 'e :> ValueType
                                        and 'e : (new : unit -> 'e)> () =

                    let e = self.Resolve<'u>()
                    builder.Enum e : App<'F,'e> |> unwrap |> Some
            }
        | _ -> None

    //------------------------------------
    // Generic F# Types
    
    let (|FSharpOption|_|) (builder : IFSharpOptionBuilder<'F>) (self : IGenericProgram<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.FSharpOption s ->
            s.Element.Accept {
                new ITypeVisitor<App<'F, 't> option> with
                    member __.Visit<'e> () =
                        let rt = self.Resolve<'e> ()
                        builder.Option rt |> unwrap |> Some
            }
    
        | _ -> None
    
    let (|FSharpList|_|) (builder : IFSharpListBuilder<'F>) (self : IGenericProgram<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.FSharpList s ->
            s.Element.Accept { new ITypeVisitor<App<'F, 't> option> with
                member __.Visit<'e>() =
                    let elem = self.Resolve<'e>()
                    builder.List elem |> unwrap |> Some }
        | _ -> None
    
    let (|FSharpSet|_|) (builder : IFSharpSetBuilder<'F>) (self : IGenericProgram<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.FSharpSet s ->
            s.Accept { new IFSharpSetVisitor<App<'F, 't> option> with
                member __.Visit<'e when 'e : comparison>() =
                    let elem = self.Resolve<'e>()
                    builder.Set elem |> unwrap |> Some 
            }
        | _ -> None
    
    let (|FSharpMap|_|) (builder : IFSharpMapBuilder<'F>) (self : IGenericProgram<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.FSharpMap s ->
            s.Accept { new IFSharpMapVisitor<App<'F, 't> option> with
                member __.Visit<'k, 'v when 'k : comparison>() =
                    let k = self.Resolve<'k>()
                    let v = self.Resolve<'v>()
                    builder.Map k v |> unwrap |> Some 
            }
        | _ -> None
    
    //------------------------------------
    // Algebraic Data Types
    
    let private handleField (self : IGenericProgram<'F>) (fieldBuilder : IFieldExtractor<'F, 'G>) (mem : IShapeMember<'t>) =
        mem.Accept {
            new IMemberVisitor<'t, App<'G, 't>> with
                member __.Visit(m : ShapeMember<'t, 'f>) =
                    let inner = self.Resolve<'f> ()
                    fieldBuilder.Field m inner
        }
    
    let (|Tuple|_|) (builder : ITupleBuilder<'F, 'G>) (self : IGenericProgram<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.Tuple (:? ShapeTuple<'t> as s) ->
            let fields = s.Elements |> Array.map (handleField self builder)
            builder.Tuple s fields |> Some
    
        | _ -> None
    
    let (|FSharpRecord|_|) (builder : IFSharpRecordBuilder<'F, 'G>) (self : IGenericProgram<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.FSharpRecord (:? ShapeFSharpRecord<'t> as s) ->
            let fields = s.Fields |> Array.map (handleField self builder)
            builder.Record s fields |> Some
    
        | _ -> None
    
    let (|FSharpUnion|_|) (builder : IFSharpUnionBuilder<'F, 'G>) (self : IGenericProgram<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.FSharpUnion (:? ShapeFSharpUnion<'t> as s) ->
            let extractCaseFields (c : ShapeFSharpUnionCase<'t>) = 
                c.Fields |> Array.map (handleField self builder)
    
            let fieldss = s.UnionCases |> Array.map extractCaseFields
            builder.Union s fieldss |> Some
    
        | _ -> None
    
    let (|CliMutable|_|) (builder : ICliMutableBuilder<'F, 'G>) (self : IGenericProgram<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.CliMutable (:? ShapeCliMutable<'t> as s) ->
            let fields = s.Properties |> Array.map (handleField self builder)
            builder.CliMutable s fields |> Some
    
        | _ -> None

    let (|Poco|_|) (builder : IPocoBuilder<'F, 'G>) (self : IGenericProgram<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.Poco (:? ShapePoco<'t> as s) ->
            let fields = s.Fields |> Array.map (handleField self builder)
            builder.Poco s fields |> Some

        | _ -> None

    let (|FSharpType|_|) (builder : IFSharpTypeBuilder<'F, 'G>) self shape : App<'F, 't> option =
        match shape with
        | Bool builder s -> Some s
        | Byte builder s -> Some s
        | SByte builder s -> Some s

        | Int16 builder s -> Some s
        | Int32 builder s -> Some s
        | Int64 builder s -> Some s

        | UInt16 builder s -> Some s
        | UInt32 builder s -> Some s
        | UInt64 builder s -> Some s

        | Single builder s -> Some s
        | Double builder s -> Some s
        | Decimal builder s -> Some s
        | BigInt builder s -> Some s

        | Unit builder s -> Some s
        | String builder s -> Some s
        | Guid builder s -> Some s

        | TimeSpan builder s -> Some s
        | DateTime builder s -> Some s
        | DateTimeOffset builder s -> Some s

        | Nullable builder self s -> Some s
        | Enum builder self s -> Some s
        | Array builder self s -> Some s

        | FSharpOption builder self s -> Some s
        | FSharpList builder self s -> Some s
        | FSharpSet builder self s -> Some s
        | FSharpMap builder self s -> Some s

        | Tuple builder self s -> Some s
        | FSharpRecord builder self s -> Some s
        | FSharpUnion builder self s -> Some s

        | _ -> None