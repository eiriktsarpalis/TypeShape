namespace TypeShape

open System
open System.Collections.Generic

open TypeShape.Core
open TypeShape.Core.Utils
open TypeShape.Core.SubtypeExtensions

module private GIterator =

    type Iterator<'Element, 'Graph> = ObjectStack -> ('Element -> unit) -> 'Graph -> unit

    let cache = new TypeCache()

    let rec mkIterator<'E, 'T> () : Iterator<'E,'T> =
        let mutable f = Unchecked.defaultof<Iterator<'E,'T>>
        if cache.TryGetValue<Iterator<'E,'T>>(&f) then f else
        use ctx = cache.CreateGenerationContext()
        mkIteratorCached<'E, 'T> ctx

    and mkIteratorCached<'E, 'T> (ctx : TypeGenerationContext) : Iterator<'E,'T> =
        match ctx.InitOrGetCachedValue<Iterator<'E,'T>> (fun c s t -> c.Value s t) with
        | Cached(value = v) -> v
        | NotCached t ->
            let iterator = mkIteratorImpl<'E, 'T> ctx
            ctx.Commit t iterator

    and mkIteratorImpl<'E, 'T> (ctx : TypeGenerationContext) : Iterator<'E, 'T> =
        let inline wrap (iterator:Iterator<'E,'a>) : Iterator<'E,'T> =
            if typeof<'a>.IsValueType then iterator
            else
                fun s f t ->
                    match t :> obj with
                    | null -> ()
                    | o ->
                        s.Push o
                        if not s.IsCycle then
                            iterator s f t
                        s.Pop()
            |> unbox

        let mkMemberIter (shape : IShapeMember<'T>) =
            shape.Accept { new IMemberVisitor<'T, Iterator<'E,'T>> with
                member __.Visit (shape : ShapeMember<'T, 'F>) =
                    let fIter = mkIteratorCached<'E, 'F> ctx
                    fun s f t -> shape.Project t |> fIter s f }

        match shapeof<'T> with
        | :? TypeShape<'E> -> wrap(fun _ f (t:'E) -> f t)
        | Shape.SubtypeOf (tshapeof<'E>) s ->
            s.Accept { new ISubtypeWitnessVisitor<'E, Iterator<'E,'T>> with
                member __.Visit(witness) =
                    wrap(fun _ f t -> f (witness.Upcast t))
            }

        | Shape.Enum s ->
            s.Accept { new IEnumVisitor<Iterator<'E,'T>> with
                member __.Visit<'t, 'u when 't : enum<'u>
                                        and 't : struct
                                        and 't :> ValueType
                                        and 't : (new : unit -> 't)>() =
                    let ui = mkIteratorCached<'E,'u> ctx
                    fun s f (t:'t) -> 
                        let u = LanguagePrimitives.EnumToValue t
                        ui s f u
                    |> wrap }

        | Shape.Nullable s ->
            s.Accept { new INullableVisitor<Iterator<'E,'T>> with
                member __.Visit<'t when 't : struct 
                                    and 't :> ValueType 
                                    and 't : (new : unit -> 't)>() =
                    let ti = mkIteratorCached<'E,'t> ctx
                    wrap(fun s f (n:Nullable<'t>) -> if n.HasValue then ti s f n.Value) }

        | Shape.FSharpOption s ->
            s.Accept {
                new IFSharpOptionVisitor<Iterator<'E,'T>> with
                    member __.Visit<'t>() =
                        let ei = mkIteratorCached<'E, 't> ctx
                        fun s f tOpt ->
                            match tOpt with
                            | None -> ()
                            | Some t -> ei s f t
                        |> wrap }

        | Shape.Array s ->
            s.Accept {
                new IArrayVisitor<Iterator<'E,'T>> with
                    member __.Visit<'t> rk =
                        let ei = mkIteratorCached<'E, 't> ctx
                        match rk with
                        | 1 -> wrap(fun s f (ts : 't[]) -> for t in ts do ei s f t)
                        | 2 -> wrap(fun s f (ts : 't[,]) -> ts |> Array2D.iter (fun t -> ei s f t))
                        | 3 -> wrap(fun s f (ts : 't[,,]) -> ts |> Array3D.iter (fun t -> ei s f t))
                        | _ -> failwithf "Rank-%d arrays not supported" rk
            }

        | Shape.FSharpList s ->
            s.Accept {
                new IFSharpListVisitor<Iterator<'E,'T>> with
                    member __.Visit<'t>() =
                        let ei = mkIteratorCached<'E, 't> ctx
                        wrap(fun s f (ts : 't list) -> for t in ts do ei s f t)
            }

        | Shape.FSharpSet s ->
            s.Accept {
                new IFSharpSetVisitor<Iterator<'E,'T>> with
                    member __.Visit<'t when 't : comparison> () =
                        let ei = mkIteratorCached<'E, 't> ctx
                        wrap(fun s f (ts:Set<'t>) -> for t in ts do ei s f t)
            }

        | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
            let eIters = shape.Elements |> Array.map mkMemberIter
            fun s f (t:'T) -> for ei in eIters do ei s f t

        | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
            let fIters = shape.Fields |> Array.map mkMemberIter
            fun s f (r:'T) -> for ei in fIters do ei s f r

        | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
            let fIterss = shape.UnionCases |> Array.map (fun uc -> uc.Fields |> Array.map mkMemberIter)
            fun s f (u:'T) ->
                let tag = shape.GetTag u
                for fI in fIterss.[tag] do fI s f u

        | Shape.Collection s ->
            s.Accept { new ICollectionVisitor<Iterator<'E,'T>> with
                member __.Visit<'Collection, 't when 'Collection :> ICollection<'t>>() =
                    let tIter = mkIteratorCached<'E,'t> ctx
                    wrap(fun s f (ts:'Collection) -> for t in ts do tIter s f t) }

        | Shape.Poco (:? ShapePoco<'T> as shape) ->
            let pIters = 
                shape.Properties 
                |> Array.filter (fun p -> p.IsPublic)
                |> Array.map mkMemberIter

            fun s f (p:'T) -> for pI in pIters do pI s f p

        | _ -> (fun _ _ _ -> ())

module private GMapper =

    type Mapper<'Element, 'Graph> = ObjectStack -> ObjectCache -> ('Element -> 'Element) -> 'Graph -> 'Graph

    let cache = new TypeCache()

    let rec mkMapper<'E, 'T> () : Mapper<'E,'T> =
        let mutable f = Unchecked.defaultof<Mapper<'E,'T>>
        if cache.TryGetValue<Mapper<'E,'T>>(&f) then f else
        use ctx = cache.CreateGenerationContext()
        mkMapperCached<'E, 'T> ctx

    and mkMapperCached<'E, 'T> (ctx : TypeGenerationContext) : Mapper<'E,'T> =
        match ctx.InitOrGetCachedValue<Mapper<'E,'T>> (fun c s t -> c.Value s t) with
        | Cached(value = v) -> v
        | NotCached t ->
            let mapper = mkMapperImpl<'E, 'T> ctx
            ctx.Commit t mapper

    and mkMapperImpl<'E, 'T> (ctx : TypeGenerationContext) : Mapper<'E, 'T> =
        let inline wrap (mapper:Mapper<'E,'a>) : Mapper<'E,'T> =
            if typeof<'a>.IsValueType then mapper
            else
                fun s (c:ObjectCache) f t ->
                    match t :> obj with
                    | null -> t
                    | o ->

                    s.Push o
                    let t' =
                        let id = s.ObjectId
                        let mutable o' : obj = null
                        if c.TryGetValue(id, &o') then o'
                        elif s.IsCycle then
                            c.CreateUninitializedInstance<obj>(id, o.GetType())
                        else
                            let t' = mapper s c f t
                            c.AddValue(id, t' :> obj)

                    s.Pop()
                    t' :?> 'a
            |> unbox

        let mkMemberMapper (shape : IShapeWriteMember<'T>) =
            shape.Accept { new IWriteMemberVisitor<'T, ObjectStack -> ObjectCache -> ('E -> 'E) -> 'T -> 'T -> 'T> with
                member __.Visit (shape : ShapeWriteMember<'T, 'F>) =
                    let fMapper = mkMapperCached<'E, 'F> ctx
                    fun s c f src tgt ->
                        let srcField = shape.Project src
                        let tgtField = fMapper s c f srcField 
                        shape.Inject tgt tgtField }

        match shapeof<'T> with
        | :? TypeShape<'E> -> wrap(fun _ _ f (t:'E) -> f t)
        | Shape.Enum s ->
            s.Accept { new IEnumVisitor<Mapper<'E,'T>> with
                member __.Visit<'t, 'u when 't : enum<'u>
                                        and 't : struct
                                        and 't :> ValueType
                                        and 't : (new : unit -> 't)>() =
                    let um = mkMapperCached<'E,'u> ctx
                    fun s c f (t:'t) -> 
                        let u = LanguagePrimitives.EnumToValue t
                        let u' = um s c f u 
                        LanguagePrimitives.EnumOfValue u'
                    |> wrap }

        | Shape.Nullable s ->
            s.Accept { new INullableVisitor<Mapper<'E,'T>> with
                member __.Visit<'t when 't : struct 
                                    and 't :> ValueType 
                                    and 't : (new : unit -> 't)>() =
                    let tm = mkMapperCached<'E,'t> ctx
                    wrap(fun s c f (n:Nullable<'t>) -> if n.HasValue then Nullable(tm s c f n.Value) else n) }

        | Shape.FSharpOption s ->
            s.Accept {
                new IFSharpOptionVisitor<Mapper<'E,'T>> with
                    member __.Visit<'t>() =
                        let em = mkMapperCached<'E, 't> ctx
                        fun s c f tOpt ->
                            match tOpt with
                            | None -> None
                            | Some t -> Some(em s c f t)
                        |> wrap }

        | Shape.Array s ->
            s.Accept {
                new IArrayVisitor<Mapper<'E,'T>> with
                    member __.Visit<'t> rk =
                        let em = mkMapperCached<'E, 't> ctx
                        match rk with
                        | 1 -> wrap(fun s c f (ts : 't[]) -> ts |> Array.map (em s c f))
                        | 2 -> wrap(fun s c f (ts : 't[,]) -> ts |> Array2D.map (em s c f))
                        | 3 -> wrap(fun s c f (ts : 't[,,]) -> ts |> Array3D.map (em s c f))
                        | _ -> failwithf "Rank-%d arrays not supported" rk
            }

        | Shape.FSharpList s ->
            s.Accept {
                new IFSharpListVisitor<Mapper<'E,'T>> with
                    member __.Visit<'t>() =
                        let em = mkMapperCached<'E, 't> ctx
                        wrap(fun s c f (ts : 't list) -> ts |> List.map (em s c f))
            }

        | Shape.FSharpSet s ->
            s.Accept {
                new IFSharpSetVisitor<Mapper<'E,'T>> with
                    member __.Visit<'t when 't : comparison> () =
                        let em = mkMapperCached<'E, 't> ctx
                        wrap(fun s c f ts -> ts |> Set.map (em s c f))
            }

        | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
            let eMappers = shape.Elements |> Array.map mkMemberMapper
            fun s c f src -> 
                let mutable tgt = shape.CreateUninitialized()
                for em in eMappers do tgt <- em s c f src tgt
                tgt

        | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
            let fMappers = shape.Fields |> Array.map mkMemberMapper
            fun s c f src ->
                let mutable tgt = shape.CreateUninitialized()
                for fm in fMappers do tgt <- fm s c f src tgt
                tgt

        | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
            let fMapperss = shape.UnionCases |> Array.map (fun uc -> uc.Fields |> Array.map mkMemberMapper)
            fun s c f src ->
                let tag = shape.GetTag src
                let case = shape.UnionCases.[tag]
                let mutable tgt = case.CreateUninitialized()
                for fm in fMapperss.[tag] do tgt <- fm s c f src tgt
                tgt

        | Shape.CliMutable (:? ShapeCliMutable<'T> as shape) ->
            let pMappers = shape.Properties |> Array.map mkMemberMapper
            fun s c f src -> 
                let mutable tgt = shape.CreateUninitialized()
                for pm in pMappers do tgt <- pm s c f src tgt
                tgt

        | _ -> (fun _ _ _ t -> t)

[<RequireQualifiedAccess>]
module Generic =

    /// <summary>
    /// Generic map combinator. Calls the mapper function on every instance
    /// of type 'a structurally within the source type 'T using depth-first
    /// traversal. Cyclic objects are supported.
    /// </summary>
    /// <param name="mapper">Mapping operation to perform on every instance of type 'a.</param>
    /// <param name="source">Source type to be traversed for instances of type 'a.</param>
    let map (mapper : 'a -> 'a) (source : 'T) : 'T =
        GMapper.mkMapper<'a,'T>() (new ObjectStack()) (new ObjectCache()) mapper source

    /// <summary>
    /// Generic iter combinator. Calls the action function on every instance
    /// of type 'a structurally wihtin the source type 'T using depth-first
    /// traversal. Cyclic objects are supported.
    /// </summary>
    /// <param name="action">Action to perform on every instance of type 'a.</param>
    /// <param name="source">Source type to be traversed for instances of type 'a.</param>
    let iter (action : 'a -> unit) (source : 'T) : unit =
        GIterator.mkIterator<'a,'T>() (new ObjectStack()) action source

    /// <summary>
    /// Generic fold combinator. Folds over instances of type 'a structurally within
    /// the source type 'T following depth-first traversal. Cyclic objects are supported.
    /// </summary>
    /// <param name="folder">Folding function for instances of type 'a.</param>
    /// <param name="state">Initial state for the fold operation.</param>
    /// <param name="source">Source type to traverse.</param>
    let fold (folder : 'State -> 'a -> 'State) (state : 'State) (source : 'T) : 'State =
        let state = ref state
        iter (fun a -> state := folder !state a) source
        !state

    /// <summary>
    /// Generic sumBy combinator. Sums instances of type 'a structurally within the source
    /// type 'T following depth-first traversal. Cyclic objects are supported.
    /// </summary>
    /// <param name="projection">Projection function to sum by.</param>
    /// <param name="source">Source type to traverse.</param>
    let inline sumBy (projection : 'a -> 'Num) (source : 'T) : 'Num =
        let count = ref LanguagePrimitives.GenericZero<'Num>
        iter (fun a -> count := !count + projection a) source
        !count

    /// <summary>
    /// Generic forall combinator. Returns true iff all instances of type 'a
    /// structurally within the source 'T satisfy the user-provided predicate.
    /// </summary>
    /// <param name="predicate">Forall predicate.</param>
    /// <param name="source">Source type to traverse.</param>
    let forall (predicate : 'a -> bool) (source : 'T) : bool =
        let mutable result = ref true
        iter (fun a -> result := !result && predicate a) source
        !result

    /// <summary>
    /// Generic exists combinator. Returns true iff at least one instance of type 'a
    /// structurally within the source 'T satisfies the user-provided predicate.
    /// </summary>
    /// <param name="predicate">Exists predicate.</param>
    /// <param name="source">Source type to traverse.</param>
    let exists (predicate : 'a -> bool) (source : 'T) : bool =
        let mutable result = ref false
        iter (fun a -> result := !result || predicate a) source
        !result

    /// <summary>
    /// Generic collect combinator. Accumulates a collection of values based on mappings
    /// of instances of type 'a structurally within the source of type 'T following
    /// depth-first traversal. Cyclic objects are supported.
    /// </summary>
    /// <param name="mapping">Mapping function for collectable values.</param>
    /// <param name="source">Source type to traverse.</param>
    let collect (mapping : 'a -> #seq<'r>) (source : 'T) : 'r[] =
        let aggregator = new ResizeArray<'r>()
        iter (mapping >> aggregator.AddRange) source
        aggregator.ToArray()