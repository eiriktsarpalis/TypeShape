namespace TypeShape.HKT

open TypeShape.Core
open TypeShape.HKT

// Interface defining the context of supported types in the samples
// Contains common F# values, DTOs with getters and setters
// and supports recursive types via IDelayBuilder.
type ITypeBuilder<'F, 'G> =
    inherit IFSharpTypeBuilder<'F, 'G>
    inherit ICliMutableBuilder<'F, 'G>
    inherit IDelayBuilder<'F>

module TypeBuilder =
    let private cache = new TypeShape.Core.Utils.TypeCache()

    /// folds ITypeBuilder instances using caching of intermediate generated values
    let fold (builder : ITypeBuilder<'F, 'G>) =
        FoldContext.fold (Some cache)
            { new IFoldContext<'F> with 
                member _.Fold<'t> self =
                    match shapeof<'t> with
                    | Fold.FSharpType builder self s -> s
                    | Fold.CliMutable builder self s -> s
                    | _ -> failwithf "Type %O not recognized as an F# data type." typeof<'t>

                member _.Delay c = builder.Delay c }