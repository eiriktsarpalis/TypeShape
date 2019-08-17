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
    FoldContext.fold cache
        { new IFoldContext<Cloner> with 
            member __.Fold<'t> self =
                match shapeof<'t> with
                | Fold.FSharpType builder self s -> s
                | Fold.CliMutable builder self s -> s
                | Fold.Poco builder self s -> s
                | _ -> failwithf "Type %A not recognized as cloneable" typeof<'t>

            member __.Delay c = builder.Delay c }

type ClonerBuilder () =
    interface IClonerBuilder with
        member __.Bool () = HKT.pack id
        member __.Byte () = HKT.pack id
        member __.SByte() = HKT.pack id
        member __.Char () = HKT.pack id

        member __.Int16 () = HKT.pack id
        member __.Int32 () = HKT.pack id
        member __.Int64 () = HKT.pack id

        member __.UInt16 () = HKT.pack id
        member __.UInt32 () = HKT.pack id
        member __.UInt64 () = HKT.pack id

        member __.Single () = HKT.pack id
        member __.Double () = HKT.pack id
        member __.Decimal() = HKT.pack id
        member __.BigInt () = HKT.pack id

        member __.Unit() = HKT.pack id
        member __.String () = HKT.pack (fun s -> if s = null then null else String.Copy s)
        member __.Guid () = HKT.pack id

        member __.TimeSpan () = HKT.pack id
        member __.DateTime () = HKT.pack id
        member __.DateTimeOffset() = HKT.pack id

        member __.Enum _ = HKT.pack id
        member __.Nullable (HKT.Unpack ec) = HKT.pack(function x when x.HasValue -> Nullable(ec x.Value) | x -> x)
        member __.Array (HKT.Unpack ec) = HKT.pack(Array.map ec)

        member __.Option (HKT.Unpack ec) = HKT.pack(Option.map ec)
        member __.List (HKT.Unpack ec) = HKT.pack(List.map ec)
        member __.Set (HKT.Unpack ec) = HKT.pack(Set.map ec)
        member __.Map (HKT.Unpack kc) (HKT.Unpack vc) = HKT.pack(Map.toSeq >> Seq.map (fun (k,v) -> kc k, vc v) >> Map.ofSeq)

        member __.Field shape (HKT.Unpack fc) = 
            HKT.pack(fun src tgt -> shape.Set tgt (fc (shape.Get src)))

        member __.Tuple shape (HKT.Unpacks fields) =
            HKT.pack(fun t ->
                let mutable t' = shape.CreateUninitialized()
                for f in fields do t' <- f t t'
                t')

        member __.Record shape (HKT.Unpacks fields) =
            HKT.pack(fun t ->
                let mutable t' = shape.CreateUninitialized()
                for f in fields do t' <- f t t'
                t')

        member __.Union shape (HKT.Unpackss fieldss) =
            HKT.pack(fun t ->
                let tag = shape.GetTag t
                let case = shape.UnionCases.[tag]
                let mutable t' = case.CreateUninitialized()
                for f in fieldss.[tag] do t' <- f t t'
                t')

        member __.CliMutable shape (HKT.Unpacks fields) =
            HKT.pack(fun t ->
                let mutable t' = shape.CreateUninitialized()
                for f in fields do t' <- f t t'
                t')

        member __.Poco shape (HKT.Unpacks fields) =
            HKT.pack(fun t ->
                let mutable t' = shape.CreateUninitialized()
                for f in fields do t' <- f t t'
                t')

        member __.Delay cell = HKT.pack(fun t -> let f = HKT.unpack cell.Value in f t)

let mkCloner<'t> () : 't -> 't = fold (ClonerBuilder()) |> HKT.unpack 
let clone t = mkCloner<'t>() t