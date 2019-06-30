namespace TypeShape.HKT

type HKT = interface end

[<Struct>]
type App<'F, 't when 'F :> HKT> = private App of payload : obj

type App<'F, 't1, 't2 when 'F :> HKT> = App<'F, TCons<'t1, 't2>>
and  App<'F, 't1, 't2, 't3 when 'F :> HKT> = App<'F, TCons<'t1, 't2, 't3>>
and  App<'F, 't1, 't2, 't3, 't4 when 'F :> HKT> = App<'F, TCons<'t1, 't2, 't3, 't4>>

and  TCons<'T1, 'T2> = class end
and  TCons<'T1, 'T2, 'T3> = TCons<TCons<'T1, 'T2>, 'T3>
and  TCons<'T1, 'T2, 'T3, 'T4> = TCons<TCons<'T1, 'T2, 'T3>, 'T4>

module HKT =

    let inline pack (value : 'Fa) : App<'F, 'a>
        when 'F : (static member Assign : App<'F, 'a> * 'Fa -> unit) =
        App<_,_>.App value
        
    let inline unpack (App value : App<'F, 'a>) : 'Fa
        when 'F : (static member Assign : App<'F, 'a> * 'Fa -> unit) =
        value :?> _
        
    // helper active patterns

    let inline (|Unpack|) app = unpack app
    let inline (|Unpacks|) apps = apps |> Seq.map unpack |> Seq.toArray
    let inline (|Unpackss|) appss = appss |> Seq.map (|Unpacks|) |> Seq.toArray