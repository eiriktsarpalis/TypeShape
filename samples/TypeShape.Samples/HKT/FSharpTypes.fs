namespace TypeShape.HKT

open TypeShape.Core
open TypeShape.Core.Utils

type IFSharpTypeBuilder<'F, 'G when 'F :> HKT and 'G :> HKT> =
    inherit IBoolBuilder<'F>

    inherit IByteBuilder<'F>
    inherit ISByteBuilder<'F>

    inherit IInt16Builder<'F>
    inherit IInt32Builder<'F>
    inherit IInt64Builder<'F>

    inherit IUInt16Builder<'F>
    inherit IUInt32Builder<'F>
    inherit IUInt64Builder<'F>

    inherit ISingleBuilder<'F>
    inherit IDoubleBuilder<'F>
    inherit IDecimalBuilder<'F>

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
    inherit IFSharpListBuilder<'F>
    inherit IFSharpMapBuilder<'F>
    inherit IFSharpSetBuilder<'F>

    inherit ITupleBuilder<'F, 'G>
    inherit IFSharpRecordBuilder<'F, 'G>
    inherit IFSharpUnionBuilder<'F, 'G>
    inherit ICliMutableBuilder<'F, 'G>

    /// Used for bootstrapping recursive types
    abstract Delay : Cell<App<'F, 't>> -> App<'F, 't>


module FSharpTypeBuilder =

    let private cache = new TypeCache()

    let rec fold builder : App<'F, 't> =
        let mutable f = Unchecked.defaultof<App<'F, 't>>
        if cache.TryGetValue(&f) then f
        else
            use ctx = cache.CreateGenerationContext()
            foldCached<'F, 'G, 't> ctx builder

    and private foldCached<'F, 'G, 't when 'F :> HKT and 'G :> HKT> (ctx : TypeGenerationContext) (builder : IFSharpTypeBuilder<'F, 'G>) : App<'F, 't> =
        match ctx.InitOrGetCachedValue<App<'F,'t>> (builder.Delay) with
        | Cached(value = f) -> f
        | NotCached t ->
            let f = foldAux<'F, 'G, 't> ctx builder
            ctx.Commit t f

    and private foldAux<'F, 'G, 't when 'F :> HKT and 'G :> HKT> (ctx : TypeGenerationContext) (builder : IFSharpTypeBuilder<'F, 'G>) : App<'F, 't> =

        let self = { new IResolver<'F> with member __.Resolve<'a> () = foldCached<'F, 'G, 'a> ctx builder }

        match tshapeof<'t> with
        | Fold.Bool builder s -> s

        | Fold.Byte builder s -> s
        | Fold.SByte builder s -> s

        | Fold.Int16 builder s -> s
        | Fold.Int32 builder s -> s
        | Fold.Int64 builder s -> s

        | Fold.UInt16 builder s -> s
        | Fold.UInt32 builder s -> s
        | Fold.UInt64 builder s -> s

        | Fold.Single builder s -> s
        | Fold.Double builder s -> s
        | Fold.Decimal builder s -> s

        | Fold.Unit builder s -> s
        | Fold.String builder s -> s
        | Fold.Guid builder s -> s
        | Fold.TimeSpan builder s -> s
        | Fold.DateTime builder s -> s
        | Fold.DateTimeOffset builder s -> s

        | Fold.Nullable builder self s -> s
        | Fold.Enum builder self s -> s
        | Fold.Array builder self s -> s

        | Fold.FSharpOption builder self s -> s
        | Fold.FSharpList builder self s -> s
        | Fold.FSharpSet builder self s -> s
        | Fold.FSharpMap builder self s -> s

        | Fold.Tuple builder self s -> s
        | Fold.FSharpRecord builder self s -> s
        | Fold.FSharpUnion builder self s -> s
        | Fold.CliMutable builder self s -> s

        | _ -> failwithf "do not know how to fold type %O" typeof<'t>