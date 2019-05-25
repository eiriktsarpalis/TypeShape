/// Generic sizeof operator
module TypeShape.Sizeof


open System
open System.Collections.Generic
open System.Runtime.Serialization

open TypeShape.Core
open TypeShape.Core.Utils

type private SizeCounter<'T> = ObjectStack -> 'T -> int64

let rec private mkSizeCounter<'T> () : SizeCounter<'T> =
    let mutable f = Unchecked.defaultof<SizeCounter<'T>>
    if cache.TryGetValue(&f) then f
    else
        use mgr = cache.CreateGenerationContext()
        mkCounterCached<'T> mgr

and private mkCounterCached<'T> (ctx : TypeGenerationContext) : SizeCounter<'T> =
    match ctx.InitOrGetCachedValue<SizeCounter<'T>> (fun c s t -> c.Value s t) with
    | Cached(value = f) -> f
    | NotCached t ->
        let f = mkCounterAux<'T> ctx
        ctx.Commit t f

and private mkCounterAux<'T> (ctx : TypeGenerationContext) : SizeCounter<'T> =
    let inline wrap (counter:SizeCounter<'a>) : SizeCounter<'T> =
        if typeof<'a>.IsValueType then counter
        else
            fun s t ->
                match t :> obj with
                | null -> 1L
                | o ->
                    s.Push o
                    let size = 
                        if s.IsCycle then 1L
                        else counter s t
                    s.Pop()
                    size
        |> unbox

    let inline mkSeqCounter (tc : SizeCounter<'t>) =
        fun s ts -> 
            let mutable c = 1L
            for t in ts do c <- c + tc s t
            c

    let mkMemberCounter (shape : IShapeMember<'DeclaringType>) =
        shape.Accept { new IMemberVisitor<'DeclaringType, SizeCounter<'DeclaringType>> with
            member __.Visit (shape : ShapeMember<'DeclaringType, 'Field>) =
                let fe = mkCounterCached<'Field> ctx
                fun s t -> fe s (shape.Get t)
        }

    match shapeof<'T> with
    | Shape.Primitive
    | Shape.Decimal
    | Shape.BigInt
    | Shape.Guid
    | Shape.TimeSpan
    | Shape.DateTime
    | Shape.Enum _
    | Shape.DateTimeOffset -> fun _ _ -> int64 sizeof<'T>
    | Shape.Unit -> fun _ _ -> 1L
    | Shape.String -> wrap(fun _ (s:string) -> int64 (sizeof<char> * s.Length))

    | Shape.Nullable s ->
        s.Accept { new INullableVisitor<SizeCounter<'T>> with
            member __.Visit<'t when 't : struct and 't :> ValueType and 't : (new : unit -> 't)>() = // 'T = 't
                let ts = mkCounterCached<'t> ctx
                fun s (n:Nullable<'t>) -> if n.HasValue then 1L + ts s n.Value else 1L
                |> wrap
        }

    | Shape.FSharpOption s ->
        s.Element.Accept { new ITypeVisitor<SizeCounter<'T>> with
            member __.Visit<'t>() = // 'T = 't option
                let tc = mkCounterCached<'t> ctx
                fun s (topt:'t option) -> match topt with None -> 1L | Some t -> 1L + tc s t
                |> wrap }

    | Shape.FSharpList s ->
        s.Element.Accept { new ITypeVisitor<SizeCounter<'T>> with
            member __.Visit<'t>() = 
                let tc = mkCounterCached<'t> ctx
                fun s (ts:'t list) -> 
                    let mutable c = 1L
                    for t in ts do c <- c + tc s t
                    c
                |> wrap }

    | Shape.SystemArray s when s.Element.Type.IsPrimitive ->
        s.Accept { new ISystemArrayVisitor<SizeCounter<'T>> with
            member __.Visit<'Array when 'Array :> System.Array>() =
                fun _ (a:'Array) -> Buffer.ByteLength a |> int64
                |> wrap
        }

    | Shape.Array s ->
        s.Element.Accept { new ITypeVisitor<SizeCounter<'T>> with
            member __.Visit<'t> () =
                let tc = mkCounterCached<'t> ctx
                match s.Rank with
                | 1 ->
                    fun s (ts:'t[]) -> 
                        let mutable c = 1L
                        for t in ts do c <- c + tc s t
                        c
                    |> wrap

                | 2 -> wrap(fun s (ts:'t[,]) -> mkSeqCounter tc s (Seq.cast ts))   // 'T = 't [,]
                | 3 -> wrap(fun s (ts:'t[,,]) -> mkSeqCounter tc s (Seq.cast ts))  // 'T = 't [,,]
                | 4 -> wrap(fun s (ts:'t[,,,]) -> mkSeqCounter tc s (Seq.cast ts)) // 'T = 't [,,,]
                | _ -> failwithf "Unsupported type %O" typeof<'T> }

    | Shape.FSharpSet s ->
        s.Accept { new IFSharpSetVisitor<SizeCounter<'T>> with
            member __.Visit<'t when 't : comparison>() = // 'T = Set<'t>
                let tc = mkCounterCached<'t> ctx
                mkSeqCounter tc : SizeCounter<Set<'t>>
                |> wrap }

    | Shape.FSharpMap s ->
        s.Accept { new IFSharpMapVisitor<SizeCounter<'T>> with
            member __.Visit<'k, 'v when 'k : comparison>() = // 'T = Map<'k,'v>
                let kvc = mkCounterCached<KeyValuePair<'k,'v>> ctx
                mkSeqCounter kvc : SizeCounter<Map<'k,'v>>
                |> wrap }

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
        let fieldCounterss = shape.UnionCases |> Array.map (fun uc -> uc.Fields |> Array.map mkMemberCounter)
        fun s (u:'T) ->
            let tag = shape.GetTag u
            let mutable c = 1L
            for fc in fieldCounterss.[tag] do c <- c + fc s u
            c

        |> wrap

    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
        let fieldCounters = shape.Elements |> Array.map mkMemberCounter
        fun s (t:'T) -> 
            let mutable c = 1L
            for fc in fieldCounters do c <- c + fc s t
            c
        |> wrap

    | Shape.Poco (:? ShapePoco<'T> as shape) ->
        let fieldCounters = shape.Fields |> Array.map mkMemberCounter
        fun s (t:'T) ->
            let mutable c = 1L
            for fc in fieldCounters do c <- c + fc s t
            c
        |> wrap

    | _ -> failwithf "Type '%O' does not support empty values." typeof<'T>

and private cache : TypeCache = new TypeCache()

/// Generic size calculator for arbitrary object graphs.
/// NB this does not denote actual memory size, it simply
/// provides a size metric that loosely corresponds to bytes.
let gsizeof<'T> (t : 'T) : int64 = mkSizeCounter<'T>() (new ObjectStack()) t