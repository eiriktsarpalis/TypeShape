module TypeShape.Empty

open System
open System.Collections.Generic

open TypeShape.Core
open TypeShape.Core.Utils

type private EmptyFunc<'T> = delegate of max:bool -> 'T
type private EmptyFieldInitializer<'T> = delegate of max:bool * value:byref<'T> -> unit

let rec private mkEmptyFunc<'T> () : EmptyFunc<'T> =
    let mutable f = Unchecked.defaultof<EmptyFunc<'T>>
    if cache.TryGetValue(&f) then f
    else
        use ctx = cache.CreateGenerationContext()
        mkEmptyFuncCached<'T> ctx

and private mkEmptyFuncCached<'T> (ctx : TypeGenerationContext) : EmptyFunc<'T> =
    match ctx.InitOrGetCachedValue<EmptyFunc<'T>>(fun c -> EmptyFunc(fun m -> c.Value.Invoke m)) with
    | Cached(isValueCreated = false) -> failwithf "Type '%O' does not support empty values." typeof<'T>
    | Cached(value = f) -> f
    | NotCached t ->
        let f = mkEmptyFuncAux<'T> ctx
        ctx.Commit t f

and private mkEmptyFuncAux<'T> (ctx : TypeGenerationContext) : EmptyFunc<'T> =
    let EQ (f : EmptyFunc<'a>) = unbox<EmptyFunc<'T>> f

    let mkMemberInitializer (shape : IShapeMember<'DeclaringType>) =
        shape.Accept { new IMemberVisitor<'DeclaringType, EmptyFieldInitializer<'DeclaringType>> with
            member _.Visit (shape : ShapeMember<'DeclaringType, 'Field>) =
                let fe = mkEmptyFuncCached<'Field> ctx
                EmptyFieldInitializer(fun m inst -> shape.SetByRef(&inst, fe.Invoke m))
        }

    match shapeof<'T> with
    | Shape.Primitive
    | Shape.TimeSpan
    | Shape.DateTime
    | Shape.DateTimeOffset
    | Shape.Decimal
    | Shape.BigInt
    | Shape.Enum _
    | Shape.Guid -> EmptyFunc(fun _ -> Unchecked.defaultof<'T>)
    | Shape.String -> EQ(EmptyFunc(fun _ -> ""))
    | Shape.Unit -> EQ(EmptyFunc(fun _ -> ()))
    | Shape.Nullable s ->
        s.Accept { new INullableVisitor<EmptyFunc<'T>> with
            member _.Visit<'t when 't : struct and 't :> ValueType and 't : (new : unit -> 't)>() = // 'T = Nullable<'t>
                let em = mkEmptyFuncCached<'t> ctx
                EQ(EmptyFunc(fun max -> if max then Nullable(em.Invoke true) else Nullable())) }

    | Shape.FSharpFunc s ->
        // empty<'T -> 'S> = fun (_ : 'T) -> empty<'S>
        s.Accept { new IFSharpFuncVisitor<EmptyFunc<'T>> with
            member _.Visit<'Dom, 'Cod> () = // 'T = 'Cod -> 'Dom
                let de = mkEmptyFuncAux<'Cod> ctx
                EQ(EmptyFunc(fun max -> fun (_ : 'Dom) -> de.Invoke max)) }

    | Shape.FSharpOption s ->
        s.Element.Accept { new ITypeVisitor<EmptyFunc<'T>> with
            member _.Visit<'t>() = // 'T = 't option
                let et = mkEmptyFuncCached<'t> ctx
                EQ(EmptyFunc(fun max -> if max then Some (et.Invoke max) else None)) }

    | Shape.KeyValuePair s ->
        s.Accept { new IKeyValuePairVisitor<EmptyFunc<'T>> with
            member _.Visit<'k,'v>() = // 'T = KeyValuePair<'k,'v>
                let ke,ve = mkEmptyFuncCached<'k> ctx, mkEmptyFuncCached<'v> ctx
                EQ(EmptyFunc(fun max -> new KeyValuePair<'k,'v>(ke.Invoke max,ve.Invoke max))) }

    | Shape.FSharpList s ->
        s.Element.Accept { new ITypeVisitor<EmptyFunc<'T>> with
            member _.Visit<'t>() = // 'T = 't list
                let te = mkEmptyFuncCached<'t> ctx
                EQ(EmptyFunc(fun max -> if max then [te.Invoke max] else [])) }

    | Shape.Array s ->
        s.Element.Accept { new ITypeVisitor<EmptyFunc<'T>> with
            member _.Visit<'t> () = 
                let te = mkEmptyFuncCached<'t> ctx
                let inline sz m = if m then 1 else 0
                match s.Rank with
                | 1 -> EQ(EmptyFunc(fun m -> if m then [|te.Invoke m|] else [||])) // 'T = 't []
                | 2 -> EQ(EmptyFunc(fun m -> let s = sz m in Array2D.init s s (fun _ _ -> te.Invoke m))) // 'T = 't [,]
                | 3 -> EQ(EmptyFunc(fun m -> let s = sz m in Array3D.init s s s (fun _ _ _ -> te.Invoke m))) // 'T = 't [,,]
                | 4 -> EQ(EmptyFunc(fun m -> let s = sz m in Array4D.init s s s s (fun _ _ _ _ -> te.Invoke m))) // 'T = 't [,,,]
                | _ -> failwithf "Unsupported type %O" typeof<'T> }

    | Shape.FSharpSet s ->
        s.Accept { new IFSharpSetVisitor<EmptyFunc<'T>> with
            member _.Visit<'t when 't : comparison>() = // 'T = Set<'t>
                let te = mkEmptyFuncCached<'t> ctx
                EQ(EmptyFunc(fun max -> if max then set [|te.Invoke max|] else Set.empty)) } 

    | Shape.FSharpMap s ->
        s.Accept { new IFSharpMapVisitor<EmptyFunc<'T>> with
            member _.Visit<'k, 'v when 'k : comparison>() = // 'T = Map<'k,'v>
                let ke, ve = mkEmptyFuncCached<'k> ctx, mkEmptyFuncCached<'v> ctx
                EmptyFunc (fun max -> 
                    if max then Map.ofArray [|(ke.Invoke max, ve.Invoke max)|] 
                    else Map.empty)
                |> EQ } 

    | Shape.Dictionary s ->
        s.Accept { new IDictionaryVisitor<EmptyFunc<'T>> with
            member _.Visit<'k, 'v when 'k : equality>() = // 'T = Dictionary<'k,'v>
                let ke, ve = mkEmptyFuncCached<'k> ctx, mkEmptyFuncCached<'v> ctx
                EmptyFunc(fun max -> 
                    let d = new Dictionary<'k,'v>()
                    if max then d.Add(ke.Invoke max, ve.Invoke max)
                    d)
                |> EQ }

    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
        let elemInitializers = shape.Elements |> Array.map mkMemberInitializer
        EmptyFunc(fun max ->
            let mutable inst = shape.CreateUninitialized()
            for f in elemInitializers do f.Invoke(max, &inst)
            inst)

    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
        let fieldInitializers = shape.Fields |> Array.map mkMemberInitializer
        EmptyFunc(fun max ->
            let mutable inst = shape.CreateUninitialized()
            for f in fieldInitializers do f.Invoke(max, &inst)
            inst)

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
        let defaultUnionCase = shape.UnionCases |> Array.minBy (fun c -> c.Fields.Length)
        let fieldInitializers = defaultUnionCase.Fields |> Array.map mkMemberInitializer
        EmptyFunc(fun max ->
            let mutable inst = defaultUnionCase.CreateUninitialized()
            for f in fieldInitializers do f.Invoke(max, &inst)
            inst)

    | Shape.CliMutable (:? ShapeCliMutable<'T> as shape) ->
        let propInitializers = shape.Properties |> Array.map mkMemberInitializer
        EmptyFunc(fun max -> 
            let mutable inst = shape.CreateUninitialized()
            if max then for p in propInitializers do p.Invoke(max, &inst)
            inst)

    | _ -> failwithf "Type '%O' does not support empty values." typeof<'T>

and private cache : TypeCache = new TypeCache()

/// Registers an empty factory for a given type
let register (emptyFactory : unit -> 'T) = cache.ForceAdd (EmptyFunc (fun (_:bool) -> emptyFactory()))

/// Generates a structural empty value for given type
let empty<'T> = mkEmptyFunc<'T>().Invoke false

/// Generates a structural empty value that populates
/// collection types with one empty element
let notEmpty<'T> = mkEmptyFunc<'T>().Invoke true