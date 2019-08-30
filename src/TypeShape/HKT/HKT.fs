namespace TypeShape.HKT

// Lightweight HKT encoding adapted from https://github.com/palladin/Higher/

open System.ComponentModel

/// HKT encoding that encapsulates an underlying materialized value.
[<Struct; StructuredFormatDisplay("{Payload}"); NoEquality; NoComparison>]
type App<'F, 't> [<EditorBrowsable(EditorBrowsableState.Never)>] (payload : obj) =
    [<EditorBrowsable(EditorBrowsableState.Never)>] 
    member __.Payload = payload

/// HKT encoding that encapsulates an underlying materialized value.
and  App<'F, 't1, 't2> = App<'F, Tup<'t1, 't2>>
/// HKT encoding that encapsulates an underlying materialized value.
and  App<'F, 't1, 't2, 't3> = App<'F, Tup<'t1, 't2, 't3>>
/// HKT encoding that encapsulates an underlying materialized value.
and  App<'F, 't1, 't2, 't3, 't4> = App<'F, Tup<'t1, 't2, 't3, 't4>>
/// HKT encoding that encapsulates an underlying materialized value.
and  App<'F, 't1, 't2, 't3, 't4, 't5> = App<'F, Tup<'t1, 't2, 't3, 't4, 't5>>

// uninhabited types encoding type-level tuples
and [<EditorBrowsable(EditorBrowsableState.Never)>] Tup<'T1, 'T2> = class end
and [<EditorBrowsable(EditorBrowsableState.Never)>] Tup<'T1, 'T2, 'T3> = class end
and [<EditorBrowsable(EditorBrowsableState.Never)>] Tup<'T1, 'T2, 'T3, 'T4> = class end
and [<EditorBrowsable(EditorBrowsableState.Never)>] Tup<'T1, 'T2, 'T3, 'T4, 'T5> = class end

module HKT =

    /// Packs a materialized value into an encoded HKT instance.
    /// Requires that the brand type 'F contains a static method Assign : App<'F, 'a> * 'Fa -> unit,
    /// so that the compiler can infer the association between encoded HKT and materialized type.
    let inline pack (value : 'Fa) : App<'F, 'a>
        when 'F : (static member Assign : App<'F, 'a> * 'Fa -> unit) =
        new App<_,_>(value)
        
    /// Unpacks an encoded HKT instance into its materialized value.
    /// Requires that the brand type 'F contains a static method Assign : App<'F, 'a> * 'Fa -> unit,
    /// so that the compiler can infer the association between encoded HKT and materialized type.
    let inline unpack (value : App<'F, 'a>) : 'Fa
        when 'F : (static member Assign : App<'F, 'a> * 'Fa -> unit) =
        value.Payload :?> _
        
    // helper active patterns

    /// Unpacks an encoded HKT instance into its materialized value.
    /// Requires that the brand type 'F contains a static method Assign : App<'F, 'a> * 'Fa -> unit,
    /// so that the compiler can infer the association between encoded HKT and materialized type.
    let inline (|Unpack|) app = unpack app

    /// Unpacks a sequence of encoded HKT instance into its materialized value.
    /// Requires that the brand type 'F contains a static method Assign : App<'F, 'a> * 'Fa -> unit,
    /// so that the compiler can infer the association between encoded HKT and materialized type.
    let inline (|Unpacks|) apps = apps |> Seq.map unpack |> Seq.toArray

    /// Unpacks a sequence of sequences of encoded HKT instance into its materialized value.
    /// Requires that the brand type 'F contains a static method Assign : App<'F, 'a> * 'Fa -> unit,
    /// so that the compiler can infer the association between encoded HKT and materialized type.
    let inline (|Unpackss|) appss = appss |> Seq.map (|Unpacks|) |> Seq.toArray