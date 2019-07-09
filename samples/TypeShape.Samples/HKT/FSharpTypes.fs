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


module Fold =
    
    let (|FSharpType|_|) self (builder : IFSharpTypeBuilder<'F, 'G>) shape : App<'F, 't> option =
        match shape with
        | Fold.Bool builder s -> Some s
        | Fold.Byte builder s -> Some s
        | Fold.SByte builder s -> Some s

        | Fold.Int16 builder s -> Some s
        | Fold.Int32 builder s -> Some s
        | Fold.Int64 builder s -> Some s

        | Fold.UInt16 builder s -> Some s
        | Fold.UInt32 builder s -> Some s
        | Fold.UInt64 builder s -> Some s

        | Fold.Single builder s -> Some s
        | Fold.Double builder s -> Some s
        | Fold.Decimal builder s -> Some s

        | Fold.Unit builder s -> Some s
        | Fold.String builder s -> Some s
        | Fold.Guid builder s -> Some s

        | Fold.TimeSpan builder s -> Some s
        | Fold.DateTime builder s -> Some s
        | Fold.DateTimeOffset builder s -> Some s

        | Fold.Nullable builder self s -> Some s
        | Fold.Enum builder self s -> Some s
        | Fold.Array builder self s -> Some s

        | Fold.FSharpOption builder self s -> Some s
        | Fold.FSharpList builder self s -> Some s
        | Fold.FSharpSet builder self s -> Some s
        | Fold.FSharpMap builder self s -> Some s

        | Fold.Tuple builder self s -> Some s
        | Fold.FSharpRecord builder self s -> Some s
        | Fold.FSharpUnion builder self s -> Some s
        | Fold.CliMutable builder self s -> Some s

        | _ -> None

module FSharpTypeBuilder =

    let fold (builder : IFSharpTypeBuilder<'F, 'G>) : App<'F, 't> =
        FoldContext.fold
            { new IFoldContext<'F> with 
                member __.Fold<'t> self = 
                    match tshapeof<'t> with
                    | Fold.FSharpType self builder s -> s
                    | _ -> failwithf "Type %O not recognized as an F# data type." typeof<'t>

                member __.Delay c = builder.Delay c }