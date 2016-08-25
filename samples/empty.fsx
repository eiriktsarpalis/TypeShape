#load "../src/TypeShape/TypeShape.fs"

// structural empty value generator

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Reflection
open TypeShape

let rec private cache = new ConcurrentDictionary<Type, obj>()
and private mkEmptyFunc<'T> () : unit -> 'T = cache.GetOrAdd(typeof<'T>, fun _ -> aux<'T> () :> obj) :?> _
and private aux<'T> () : unit -> 'T =  
    let wrap (f : unit -> 'a) = unbox<unit -> 'T> f
    match TypeShape.Create<'T>() with
    | Shape.Bool -> wrap(fun () -> false)
    | Shape.Byte -> wrap(fun () -> 0uy)
    | Shape.SByte -> wrap(fun () -> 0y)
    | Shape.Int16 -> wrap(fun () -> 0s)
    | Shape.Int32 -> wrap(fun () -> 0)
    | Shape.Int64 -> wrap(fun () -> 0L)
    | Shape.UInt16 -> wrap(fun () -> 0us)
    | Shape.UInt32 -> wrap(fun () -> 0u)
    | Shape.UInt64 -> wrap(fun () -> 0uL)
    | Shape.Decimal -> wrap(fun () -> 0M)
    | Shape.Single -> wrap(fun () -> 0.f)
    | Shape.Double -> wrap(fun () -> 0.)
    | Shape.Guid -> wrap(fun () -> Guid.Empty)
    | Shape.Char -> wrap(fun () -> Unchecked.defaultof<char>)
    | Shape.String -> wrap(fun () -> "")
    | Shape.TimeSpan -> wrap(fun () -> TimeSpan.Zero)
    | Shape.DateTime -> wrap(fun () -> DateTime.MinValue)
    | Shape.DateTimeOffset -> wrap(fun () -> DateTimeOffset.MinValue)
    | Shape.Unit -> wrap id
    | Shape.Tuple2 s ->
        s.Accept { new ITuple2Visitor<unit -> 'T> with
            member __.Visit<'t1,'t2> () =
                let et1,et2 = mkEmptyFunc<'t1>(), mkEmptyFunc<'t2>()
                wrap(fun () -> et1 (), et2()) }

    | Shape.Tuple3 s ->
        s.Accept { new ITuple3Visitor<unit -> 'T> with
            member __.Visit<'t1,'t2,'t3> () =
                let et1,et2,et3 = mkEmptyFunc<'t1>(), mkEmptyFunc<'t2>(), mkEmptyFunc<'t3>()
                wrap(fun () -> et1 (), et2(), et3()) }

    | Shape.FSharpOption s ->
        s.Accept { new IFSharpOptionVisitor<unit -> 'T> with
            member __.Visit<'t>() =
                let et = mkEmptyFunc<'t>()
                wrap(fun () -> Option<'t>.None) }

    | Shape.FSharpList s ->
        s.Accept { new IFSharpListVisitor<unit -> 'T> with
            member __.Visit<'t>() = wrap(fun () -> List.empty<'t>) }

    | Shape.Array s ->
        s.Accept { new IArrayVisitor<unit -> 'T> with
            member __.Visit<'t>() = wrap(fun () -> Array.empty<'t>) }

    | Shape.FSharpSet s ->
        s.Accept { new IFSharpSetVisitor<unit -> 'T> with
            member __.Visit<'t when 't : comparison>() = wrap(fun () -> Set.empty<'t>) }

    | Shape.FSharpMap s ->
        s.Accept { new IFSharpMapVisitor<unit -> 'T> with
            member __.Visit<'K, 'V when 'K : comparison>() = wrap(fun () -> Map.empty<'K,'V>) }

    | Shape.KeyValuePair s ->
        s.Accept { new IKeyValuePairVisitor<unit -> 'T> with
            member __.Visit<'K,'V>() = 
                let kt,vt = mkEmptyFunc<'K>(), mkEmptyFunc<'V>()
                wrap(fun () -> new KeyValuePair<'K,'V>(kt(),vt())) }

    | Shape.FSharpRecord s as ts ->
        let mkEmptyField (p : PropertyInfo) =
            TypeShape.Create(p.PropertyType).Accept 
                { new ITypeShapeVisitor<unit -> obj> with
                    member __.Visit<'T>() =
                        let et = mkEmptyFunc<'T>()
                        fun () -> et () :> obj }

        ts.Accept { new ITypeShapeVisitor<unit -> 'T> with
            member __.Visit<'t>() =
                let fieldCtors = s.Properties |> Seq.map mkEmptyField |> Seq.toArray
                wrap (fun () -> fieldCtors |> Array.map (fun f -> f ()) |> s.ConstructorInfo.Invoke :?> 't) }

    | _ -> failwithf "Type '%O' does not support empty types." typeof<'T>

and empty<'T> = mkEmptyFunc<'T> () ()

/// examples

type Record<'T> =
    {
        Id : string
        Value : 'T ref
        UUID : Guid
        MoneyMoney : decimal
        Values : (int * string) list
        Metrics : Map<string, float>
        Set : Set<string>
    }

{ empty<Record<int * string option>> with Id = "myId" ; MoneyMoney = 3.14M }