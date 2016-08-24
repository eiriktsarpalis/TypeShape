#load "../src/TypeShape/TypeShape.fs"

// structural empty value generator

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Reflection
open TypeShape

let rec private cache = new ConcurrentDictionary<Type, obj>()
and private mkEmptyFunc<'T> () : unit -> 'T = cache.GetOrAdd(typeof<'T>, mkEmptyFuncUntyped) :?> _
and private mkEmptyFuncUntyped (t : Type) : obj =
    let wrap (f : unit -> 'a) = box f
    match TypeShape.Create t with
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
    | :? TypeShape<bigint> -> wrap(fun () -> 0I)
    | Shape.Tuple2 s ->
        s.Accept { new ITuple2Visitor<obj> with
            member __.Visit<'T1,'T2> () =
                let et1,et2 = mkEmptyFunc<'T1>(), mkEmptyFunc<'T2>()
                wrap(fun () -> et1 (), et2()) }

    | Shape.Tuple3 s ->
        s.Accept { new ITuple3Visitor<obj> with
            member __.Visit<'T1,'T2, 'T3> () =
                let et1,et2,et3 = mkEmptyFunc<'T1>(), mkEmptyFunc<'T2>(), mkEmptyFunc<'T3>()
                wrap(fun () -> et1 (), et2(), et3()) }

    | Shape.FSharpOption s ->
        s.Accept { new IFSharpOptionVisitor<obj> with
            member __.Visit<'T>() =
                let et = mkEmptyFunc<'T>()
                wrap(fun () -> Option<'T>.None) }

    | Shape.FSharpList s ->
        s.Accept { new IFSharpListVisitor<obj> with
            member __.Visit<'T>() = wrap(fun () -> List.empty<'T>) }

    | Shape.Array s ->
        s.Accept { new IArrayVisitor<obj> with
            member __.Visit<'T>() = wrap(fun () -> Array.empty<'T>) }

    | Shape.FSharpSet s ->
        s.Accept { new IFSharpSetVisitor<obj> with
            member __.Visit<'T when 'T : comparison>() = wrap(fun () -> Set.empty<'T>) }

    | Shape.FSharpMap s ->
        s.Accept { new IFSharpMapVisitor<obj> with
            member __.Visit<'K, 'V when 'K : comparison>() = wrap(fun () -> Map.empty<'K,'V>) }

    | Shape.KeyValuePair s ->
        s.Accept { new IKeyValuePairVisitor<obj> with
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

        ts.Accept { new ITypeShapeVisitor<obj> with
            member __.Visit<'T>() =
                let fieldCtors = s.Properties |> Seq.map mkEmptyField |> Seq.toArray
                wrap (fun () -> fieldCtors |> Array.map (fun f -> f ()) |> s.ConstructorInfo.Invoke :?> 'T) }

    | _ -> failwithf "Type '%O' does not support empty types." t

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

{ empty<Record<int * string option>> with Id = "myId" }