#r "../bin/TypeShape.dll"

// structural empty value generator

open System
open System.Collections.Generic
open System.Runtime.Serialization

open TypeShape
open TypeShape_Utils

type SizeCounter<'T> = 
    abstract Compute : ObjectIDGenerator -> Dictionary<int64, int64> -> 'T -> int64

let rec mkSizeCounter<'T> () : SizeCounter<'T> =
    let mutable f = Unchecked.defaultof<SizeCounter<'T>>
    if cache.TryGetValue(&f) then f
    else
        use mgr = cache.CreateRecTypeManager()
        mkCounterCached<'T> mgr

and private mkCounterCached<'T> (ctx : RecTypeManager) : SizeCounter<'T> =
    match ctx.TryFind<SizeCounter<'T>>() with
    | Some f -> f
    | None ->
        let _ = ctx.CreateUninitialized<SizeCounter<'T>>(fun c -> 
                    { new SizeCounter<'T> with 
                        member __.Compute g d t = c.Value.Compute g d t })

        let f = mkCounterAux<'T> ctx
        ctx.Complete f

and private mkCounterAux<'T> (ctx : RecTypeManager) : SizeCounter<'T> =
    let EQ(sc : SizeCounter<'a>) = unbox<SizeCounter<'T>> sc
    let inline wrap (f : _ -> _ -> 'a -> _) = 
        let sc = { new SizeCounter<'a> with member __.Compute g d t = f g d t }
        unbox<SizeCounter<'a>> sc

    let inline withCycleCheck sc (g : ObjectIDGenerator) (d : Dictionary<int64,int64>) (t:'a) =
        let mutable firstTime = false
        let id = g.GetId(t, &firstTime)
        if firstTime then
            let sz = sc g d t
            d.[id] <- sz
            sz
        else
            let ok,found = d.TryGetValue id
            if ok then found
            else 1L // found cyclic graph

    let inline mkSeqCounter (tc : SizeCounter<'t>) =
        fun g d ts -> 
            let mutable c = 0L
            for t in ts do c <- c + tc.Compute g d t
            c
        |> withCycleCheck

    let mkMemberCounter (shape : IShapeWriteMember<'DeclaringType>) =
        shape.Accept { new IWriteMemberVisitor<'DeclaringType, SizeCounter<'DeclaringType>> with
            member __.Visit (shape : ShapeWriteMember<'DeclaringType, 'Field>) =
                let fe = mkCounterCached<'Field> ctx
                fun g d t -> fe.Compute g d (shape.Project t)
                |> wrap
        }

    match shapeof<'T> with
    | Shape.Primitive
    | Shape.Decimal
    | Shape.BigInt
    | Shape.Guid
    | Shape.TimeSpan
    | Shape.DateTime
    | Shape.Enum _
    | Shape.DateTimeOffset -> let sz = int64 sizeof<'T> in wrap(fun _ _ _ -> sz)
    | Shape.Unit -> wrap(fun _ _ _ -> 0L)
    | Shape.String -> 
        let sc = int64 sizeof<char>
        wrap(fun _ _ (s:string) -> sc * int64 s.Length) |> EQ

    | Shape.Nullable s ->
        s.Accept { new INullableVisitor<SizeCounter<'T>> with
            member __.Visit<'t when 't : struct and 't :> ValueType and 't : (new : unit -> 't)>() = // 'T = 't
                let ts = mkCounterCached<'t> ctx
                wrap(fun g d (n:Nullable<'t>) -> if n.HasValue then 1L else 1L + ts.Compute g d n.Value)
                |> EQ
        }

    | Shape.FSharpOption s ->
        s.Accept { new IFSharpOptionVisitor<SizeCounter<'T>> with
            member __.Visit<'t>() = // 'T = 't option
                let tc = mkCounterCached<'t> ctx
                fun g d (topt:'t option) -> match topt with None -> 1L | Some t -> 1L + tc.Compute g d t
                |> withCycleCheck
                |> wrap
                |> EQ }

    | Shape.FSharpList s ->
        s.Accept { new IFSharpListVisitor<SizeCounter<'T>> with
            member __.Visit<'t>() = 
                let tc = mkCounterCached<'t> ctx
                fun g d (ts:'t list) -> 
                    let mutable c = 0L
                    for t in ts do c <- c + tc.Compute g d t
                    c
                |> withCycleCheck
                |> wrap
                |> EQ }

    | Shape.SystemArray s when s.Element.Type.IsPrimitive ->
        s.Accept { new ISystemArrayVisitor<SizeCounter<'T>> with
            member __.Visit<'Array when 'Array :> System.Array>() =
                fun g d (a:'Array) -> Buffer.ByteLength a |> int64
                |> wrap
                |> EQ
        }

    | Shape.Array s ->
        s.Accept { new IArrayVisitor<SizeCounter<'T>> with
            member __.Visit<'t> rank =
                let tc = mkCounterCached<'t> ctx
                match rank with
                | 1 -> 
                    fun g d (ts:'t[]) -> 
                        let mutable c = 0L
                        for t in ts do c <- c + tc.Compute g d t
                        c
                    |> withCycleCheck 
                    |> wrap
                    |> EQ

                | 2 -> EQ(wrap(fun g d (ts:'t[,]) -> mkSeqCounter tc g d (Seq.cast ts)))   // 'T = 't [,]
                | 3 -> EQ(wrap(fun g d (ts:'t[,,]) -> mkSeqCounter tc g d (Seq.cast ts)))  // 'T = 't [,,]
                | 4 -> EQ(wrap(fun g d (ts:'t[,,,]) -> mkSeqCounter tc g d (Seq.cast ts))) // 'T = 't [,,,]
                | _ -> failwithf "Unsupported type %O" typeof<'T> }

    | Shape.FSharpSet s ->
        s.Accept { new IFSharpSetVisitor<SizeCounter<'T>> with
            member __.Visit<'t when 't : comparison>() = // 'T = Set<'t>
                let tc = mkCounterCached<'t> ctx
                mkSeqCounter tc
                |> wrap : SizeCounter<Set<'t>>
                |> EQ }

    | Shape.FSharpMap s ->
        s.Accept { new IFSharpMapVisitor<SizeCounter<'T>> with
            member __.Visit<'k, 'v when 'k : comparison>() = // 'T = Map<'k,'v>
                let kvc = mkCounterCached<KeyValuePair<'k,'v>> ctx
                mkSeqCounter kvc
                |> wrap : SizeCounter<Map<'k,'v>>
                |> EQ }

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
        let fieldCounterss = shape.UnionCases |> Array.map (fun uc -> uc.Fields |> Array.map mkMemberCounter)
        fun g d (u:'T) ->
            let tag = shape.GetTag u
            let mutable c = 1L
            for fc in fieldCounterss.[tag] do c <- c + fc.Compute g d u
            c
        |> withCycleCheck
        |> wrap

    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
        let fieldCounters = shape.Elements |> Array.map mkMemberCounter
        fun g d (t:'T) -> 
            let mutable c = 0L
            for fc in fieldCounters do c <- c + fc.Compute g d t
            c
        |> withCycleCheck
        |> wrap

    | Shape.Poco (:? ShapePoco<'T> as shape) ->
        let fieldCounters = shape.Fields |> Array.map mkMemberCounter
        let getSize g d (t:'T) = 
            let mutable c = 0L
            for fc in fieldCounters do c <- c + fc.Compute g d t
            c

        if shape.IsStruct then getSize
        else getSize |> withCycleCheck
        |> wrap

    | _ -> failwithf "Type '%O' does not support empty values." typeof<'T>

and private cache : TypeCache = new TypeCache()

/// Generates a structural empty value for given type
let gsizeof<'T>(t : 'T) = mkSizeCounter<'T>().Compute (new ObjectIDGenerator()) (Dictionary()) t


//-------------------------
// examples

gsizeof ()
gsizeof (Some 2)
gsizeof 1
gsizeof 1L
gsizeof "string"
gsizeof [[1 .. 100]; [101 .. 200]]
gsizeof [|1L;2L;3L|]
gsizeof [false;true;false;true]

// recursive types

type P = Z | S of P

gsizeof (S(S(S(S Z))))

gsizeof Z

// cyclic objects

type Rec = Rec of (Rec -> Rec)

let rec r = Rec(function (Rec f) -> f r)

gsizeof r