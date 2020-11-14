module TypeShape.HKT.Empty

open System

// HKT encoding for prettyprinter types
type Empty =
    static member Assign(_ : App<Empty, 'a>, _ : unit -> 'a) = ()

// HKT encoding for field accessor
type EmptyFieldUpdater =
    static member Assign(_ : App<EmptyFieldUpdater, 'a>, _ : 'a -> 'a) = ()

type PrettyPrinterBuilder() =
    interface ITypeBuilder<Empty, EmptyFieldUpdater> with
        member _.Bool () = HKT.pack(fun () -> false)
        member _.Byte () = HKT.pack(fun () -> 0uy)
        member _.SByte() = HKT.pack(fun () -> 0y)
        member _.Char () = HKT.pack(fun () -> '\u0000')
        
        member _.Int16 () = HKT.pack(fun () -> 0s)
        member _.Int32 () = HKT.pack(fun () -> 0)
        member _.Int64 () = HKT.pack(fun () -> 0L)

        member _.UInt16 () = HKT.pack(fun () -> 0us)
        member _.UInt32 () = HKT.pack(fun () -> 0u)
        member _.UInt64 () = HKT.pack(fun () -> 0uL)

        member _.Single () = HKT.pack(fun () -> 0.f)
        member _.Double () = HKT.pack(fun () -> 0.)
        member _.Decimal() = HKT.pack(fun () -> 0m)
        member _.BigInt () = HKT.pack(fun () -> 0I)

        member _.Unit() = HKT.pack id
        member _.String () = HKT.pack(fun () -> "")
        member _.Guid () = HKT.pack(fun () -> Guid.Empty)

        member _.TimeSpan () = HKT.pack(fun () -> TimeSpan.Zero)
        member _.DateTime () = HKT.pack(fun () -> DateTime.MinValue)
        member _.DateTimeOffset() = HKT.pack(fun () -> DateTimeOffset.MinValue)

        member _.Enum (HKT.Unpack e) = HKT.pack (fun () -> LanguagePrimitives.EnumOfValue(e ()))
        member _.Nullable _ = HKT.pack (fun () -> Nullable())
        member _.Array _ = HKT.pack(fun () -> [||])

        member _.Option _ = HKT.pack(fun () -> None)
        member _.List _ = HKT.pack(fun () -> [])
        member _.Set _ = HKT.pack(fun () -> Set.empty)
        member _.Map _ _ = HKT.pack(fun () -> Map.empty)

        member _.Field shape (HKT.Unpack fp) = 
            HKT.pack(fun t -> shape.Set t (fp()))

        member _.Tuple shape (HKT.Unpacks fields) =
            HKT.pack(fun () ->
                let mutable t = shape.CreateUninitialized()
                for f in fields do t <- f t
                t)

        member _.Record shape (HKT.Unpacks fields) =
            HKT.pack(fun () ->
                let mutable t = shape.CreateUninitialized()
                for f in fields do t <- f t
                t)

        member _.Union shape (HKT.Unpackss fieldss) =
            let tag,case = shape.UnionCases |> Seq.mapi (fun i c -> (i,c)) |> Seq.minBy (fun (_,c) -> c.Arity)
            let fields = fieldss.[tag]
            HKT.pack(fun () ->
                let mutable t = case.CreateUninitialized()
                for f in fields do t <- f t
                t)

        member _.CliMutable shape (HKT.Unpacks fields) =
            HKT.pack(fun () ->
                let mutable t = shape.CreateUninitialized()
                for f in fields do t <- f t
                t)

        member _.Delay _ = HKT.pack(fun () -> Unchecked.defaultof<_>)

let mkEmpty<'t> () : unit -> 't = TypeBuilder.fold (PrettyPrinterBuilder()) |> HKT.unpack 
let empty<'t> = mkEmpty<'t> () ()

//----------------------

open System

type P = Z | S of P

empty<int * (string * DateTime) option * {| x : int ; p : P |}>