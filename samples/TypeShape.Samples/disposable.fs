module Disposable

open System
open TypeShape.Core
open TypeShape.Core.Utils
open TypeShape.Core.SubtypeExtensions

// Generic IDisposable generator

let rec mkDisposer<'T> () : 'T -> unit =
    let mutable f = Unchecked.defaultof<'T -> unit>
    if cache.TryGetValue(&f) then f
    else
        use mgr = cache.CreateGenerationContext()
        mkDisposerCached<'T> mgr

and private mkDisposerCached<'T> (ctx : TypeGenerationContext) : 'T -> unit =
    match ctx.InitOrGetCachedValue<'T -> unit>(fun c t -> c.Value t) with
    | Cached(value = f) -> f
    | NotCached t ->
        let f = mkDisposerAux<'T> ctx
        ctx.Commit t f

and private mkDisposerAux<'T> (ctx : TypeGenerationContext) : 'T -> unit =
    let EQ (f : 'a -> unit) = unbox<'T -> unit> f

    let mkMemberDisposer (shape : IShapeMember<'DeclaringType>) =
        shape.Accept { new IMemberVisitor<'DeclaringType, 'DeclaringType -> unit> with
            member _.Visit (shape : ShapeMember<'DeclaringType, 'Field>) =
                let fd = mkDisposerCached<'Field> ctx
                fun inst -> let f = shape.Get inst in fd f }

    match shapeof<'T> with
    | Shape.IDisposable s ->
        s.Accept { new ISubtypeVisitor<IDisposable, ('T -> unit)> with
            member _.Visit<'D when 'D :> IDisposable> () =
                if typeof<'D>.IsValueType then
                    fun (d:'D) -> d.Dispose()
                else
                    fun (d:'D) -> if not(obj.ReferenceEquals(d,null)) then d.Dispose()
                |> EQ }

    | Shape.Nullable s ->
        s.Accept { new INullableVisitor<'T -> unit> with
            member _.Visit<'t when 't : struct and 't :> ValueType and 't : (new : unit -> 't)>() = // 'T = 't
                let td = mkDisposerCached<'t> ctx
                EQ (fun (t : Nullable<'t>) -> if t.HasValue then td t.Value)
        }

    | Shape.FSharpList s ->
        s.Element.Accept { new ITypeVisitor<'T -> unit> with
            member _.Visit<'t>() = // 'T = 't list
                let td = mkDisposerCached<'t> ctx
                EQ (fun (ts : 't list) -> for t in ts do td t) } 

    | Shape.Array s when s.Rank = 1 ->
        s.Element.Accept { new ITypeVisitor<'T -> unit> with
            member _.Visit<'t> () = // 'T = 't []
                let td = mkDisposerCached<'t> ctx
                EQ (fun (ts : 't []) -> for t in ts do td t) } 

    | Shape.FSharpSet s ->
        s.Accept { new IFSharpSetVisitor<'T -> unit> with
            member _.Visit<'t when 't : comparison>() = // 'T = Set<'t>
                let td = mkDisposerCached<'t> ctx
                EQ (fun (ts : Set<'t>) -> for t in ts do td t) } 

    | Shape.FSharpMap s ->
        s.Accept { new IFSharpMapVisitor<'T -> unit> with
            member _.Visit<'k, 'v when 'k : comparison>() = // 'T = Map<'k,'v>
                let kd, vd = mkDisposerCached<'k> ctx, mkDisposerCached<'v> ctx
                EQ(fun (m : Map<'k,'v>) -> for kv in m do kd kv.Key ; vd kv.Value) }

    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
        let elemDisposers = shape.Elements |> Array.map mkMemberDisposer
        fun t -> for d in elemDisposers do d t

    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
        let fieldDisposers = shape.Fields |> Array.map mkMemberDisposer
        fun t -> for d in fieldDisposers do d t

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
        let fieldDisposers = shape.UnionCases |> Array.map (fun c -> Array.map mkMemberDisposer c.Fields)
        fun t ->
            let tag = shape.GetTag t
            for d in fieldDisposers.[tag] do d t

    | _ -> ignore

and private cache : TypeCache = new TypeCache()

/// Performs a structural disposal of provided type
let dispose (t : 'T) = mkDisposer<'T> () t
/// Creates an IDisposable token that structurally disposes contents
let mkDisposable (t : 'T) = { new IDisposable with member _.Dispose() = dispose t }


//-------------------------
// examples

type Disposable() =
    static let mutable counter = 0
    let id = System.Threading.Interlocked.Increment &counter
    interface IDisposable with 
        member _.Dispose() = printfn "Disposing %d" id
    
let d() = new Disposable()


dispose [d() ; d(); d()]
dispose [Some(d()) ; None ; Some(d())]


type Tree<'T> = Leaf | Node of 'T * Tree<'T> * Tree<'T>

dispose <| Node(d(), Leaf, Node(d(), Leaf, Leaf))