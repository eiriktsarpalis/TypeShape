namespace TypeShape.HKT

open TypeShape.Core
open TypeShape.HKT

type ITypeBuilder<'F, 'G> =
    inherit IFSharpTypeBuilder<'F, 'G>
    inherit ICliMutableBuilder<'F, 'G>
    inherit IDelayBuilder<'F>

module TypeBuilder =
    let fold (builder : ITypeBuilder<'F, 'G>) =
        FoldContext.fold
            { new IFoldContext<'F> with 
                member __.Fold<'t> self =
                    match shapeof<'t> with
                    | Fold.FSharpType builder self s -> s
                    | Fold.CliMutable builder self s -> s
                    | _ -> failwithf "Type %O not recognized as an F# data type." typeof<'t>

                member __.Delay c = builder.Delay c }