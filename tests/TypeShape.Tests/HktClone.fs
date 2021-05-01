module TypeShape.Tests.HktClone

open System
open TypeShape.Core
open TypeShape.HKT

// HKT encoding for prettyprinter types
type Cloner =
    static member Assign(_ : App<Cloner, 'a>, _ : 'a -> 'a) = ()

type FieldCloner =
    static member Assign(_ : App<FieldCloner, 'a>, _ : 'a -> 'a -> 'a) = ()

type IClonerBuilder =
    inherit IFSharpTypeBuilder<Cloner, FieldCloner>
    inherit ICliMutableBuilder<Cloner, FieldCloner>
    inherit IPocoBuilder<Cloner, FieldCloner>
    inherit IDelayBuilder<Cloner>

let private cache = new TypeShape.Core.Utils.TypeCache()

let fold (builder : IClonerBuilder) =
    FoldContext.fold (Some cache)
        { new IFoldContext<Cloner> with 
            member _.Fold<'t> self =
                match shapeof<'t> with
                | Fold.FSharpType builder self s -> s
                | Fold.CliMutable builder self s -> s
                | Fold.Poco builder self s -> s
                | _ -> failwithf "Type %A not recognized as cloneable" typeof<'t>

            member _.Delay c = builder.Delay c }

type ClonerBuilder () =
    interface IClonerBuilder with
        member _.Bool () = HKT.pack id
        member _.Byte () = HKT.pack id
        member _.SByte() = HKT.pack id
        member _.Char () = HKT.pack id

        member _.Int16 () = HKT.pack id
        member _.Int32 () = HKT.pack id
        member _.Int64 () = HKT.pack id

        member _.UInt16 () = HKT.pack id
        member _.UInt32 () = HKT.pack id
        member _.UInt64 () = HKT.pack id

        member _.Single () = HKT.pack id
        member _.Double () = HKT.pack id
        member _.Decimal() = HKT.pack id
        member _.BigInt () = HKT.pack id

        member _.Unit() = HKT.pack id
        member _.String () = HKT.pack id
        member _.Guid () = HKT.pack id

        member _.TimeSpan () = HKT.pack id
        member _.DateTime () = HKT.pack id
        member _.DateTimeOffset() = HKT.pack id

        member _.Enum _ = HKT.pack id
        member _.Nullable (HKT.Unpack ec) = HKT.pack(function x when x.HasValue -> Nullable(ec x.Value) | x -> x)
        member _.Array (HKT.Unpack ec) = HKT.pack(Array.map ec)

        member _.Option (HKT.Unpack ec) = HKT.pack(Option.map ec)
        member _.Ref (HKT.Unpack ec) = HKT.pack(fun r -> ref (ec r.Value))
        member _.List (HKT.Unpack ec) = HKT.pack(List.map ec)
        member _.Set (HKT.Unpack ec) = HKT.pack(Set.map ec)
        member _.Map (HKT.Unpack kc) (HKT.Unpack vc) = HKT.pack(Map.toSeq >> Seq.map (fun (k,v) -> kc k, vc v) >> Map.ofSeq)

        member _.Field shape (HKT.Unpack fc) = 
            HKT.pack(fun src tgt -> shape.Set tgt (fc (shape.Get src)))

        member _.Tuple shape (HKT.Unpacks fields) =
            HKT.pack(fun t ->
                let mutable t' = shape.CreateUninitialized()
                for f in fields do t' <- f t t'
                t')

        member _.Record shape (HKT.Unpacks fields) =
            HKT.pack(fun t ->
                let mutable t' = shape.CreateUninitialized()
                for f in fields do t' <- f t t'
                t')

        member _.Union shape (HKT.Unpackss fieldss) =
            HKT.pack(fun t ->
                let tag = shape.GetTag t
                let case = shape.UnionCases.[tag]
                let mutable t' = case.CreateUninitialized()
                for f in fieldss.[tag] do t' <- f t t'
                t')

        member _.CliMutable shape (HKT.Unpacks fields) =
            HKT.pack(fun t ->
                let mutable t' = shape.CreateUninitialized()
                for f in fields do t' <- f t t'
                t')

        member _.Poco shape (HKT.Unpacks fields) =
            HKT.pack(fun t ->
                let mutable t' = shape.CreateUninitialized()
                for f in fields do t' <- f t t'
                t')

        member _.Delay cell = HKT.pack(fun t -> let f = HKT.unpack cell.Value in f t)

let mkCloner<'t> () : 't -> 't = fold (ClonerBuilder()) |> HKT.unpack 
let clone t = mkCloner<'t>() t
