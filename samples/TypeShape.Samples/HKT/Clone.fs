module TypeShape.HKT.Clone

open System

// HKT encoding for prettyprinter types
type Cloner =
    static member Assign(_ : App<Cloner, 'a>, _ : 'a -> 'a) = ()

// HKT encoding for field cloning accessor
type FieldCloner =
    static member Assign(_ : App<FieldCloner, 'a>, _ : 'a -> 'a -> 'a) = ()

type ClonerBuilder() =
    interface ITypeBuilder<Cloner, FieldCloner> with
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

        member _.Delay cell = HKT.pack(fun t -> let f = HKT.unpack cell.Value in f t)

let mkCloner<'t> () : 't -> 't = TypeBuilder.fold (ClonerBuilder()) |> HKT.unpack 
let clone t = mkCloner<'t>() t

//----------------------

open System

type P = Z | S of P

clone (42, Some "42", {| x = 2 ; y = S(S(S Z)) |})