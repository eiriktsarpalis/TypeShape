module TypeShape.HKT.Empty

open System

// HKT encoding for prettyprinter types
type Empty =
    interface HKT
    static member Assign(_ : App<Empty, 'a>, _ : unit -> 'a) = ()

type EmptyFieldUpdater =
    interface HKT
    static member Assign(_ : App<EmptyFieldUpdater, 'a>, _ : 'a -> 'a) = ()

type PrettyPrinterBuilder() =
    interface IFSharpTypeBuilder<Empty, EmptyFieldUpdater> with
        member __.Unit() = HKT.pack id
        member __.Bool () = HKT.pack(fun () -> false)
        member __.Int32 () = HKT.pack(fun () -> 0)
        member __.Int64 () = HKT.pack(fun () -> 0L)
        member __.String () = HKT.pack(fun () -> "")

        member __.Guid () = HKT.pack(fun () -> Guid.Empty)
        member __.TimeSpan () = HKT.pack(fun () -> TimeSpan.Zero)
        member __.DateTime () = HKT.pack(fun () -> DateTime.MinValue)
        member __.DateTimeOffset() = HKT.pack(fun () -> DateTimeOffset.MinValue)

        member __.Option _ = HKT.pack(fun () -> None)
        member __.Array _ = HKT.pack(fun () -> [||])
        member __.List _ = HKT.pack(fun () -> [])
        member __.Set _ = HKT.pack(fun () -> Set.empty)
        member __.Map _ _ = HKT.pack(fun () -> Map.empty)

        member __.Field shape (HKT.Unpack fp) = 
            HKT.pack(fun t -> shape.Set t (fp()))

        member __.Tuple shape (HKT.Unpacks fields) =
            HKT.pack(fun () ->
                let mutable t = shape.CreateUninitialized()
                for f in fields do t <- f t
                t)

        member __.Record shape (HKT.Unpacks fields) =
            HKT.pack(fun () ->
                let mutable t = shape.CreateUninitialized()
                for f in fields do t <- f t
                t)

        member __.Union shape (HKT.Unpackss fieldss) =
            let tag,case = shape.UnionCases |> Seq.mapi (fun i c -> (i,c)) |> Seq.minBy (fun (_,c) -> c.Arity)
            let fields = fieldss.[tag]
            HKT.pack(fun () ->
                let mutable t = case.CreateUninitialized()
                for f in fields do t <- f t
                t)

        member __.Delay _ = HKT.pack(fun () -> Unchecked.defaultof<_>)

let mkEmpty<'t> () : unit -> 't = FSharpTypeBuilder.fold (PrettyPrinterBuilder()) |> HKT.unpack 
let empty<'t> = mkEmpty<'t> () ()

//----------------------

open System

type P = Z | S of P

empty<int * (string * DateTime) option * {| x : int ; p : P |}>