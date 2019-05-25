module TypeShape.Empty

open System
open System.Collections.Generic

open TypeShape.Core
open TypeShape.Core.Utils

let rec private mkEmptyFunc<'T> () : bool -> 'T =
    let mutable f = Unchecked.defaultof<bool -> 'T>
    if cache.TryGetValue(&f) then f
    else
        use ctx = cache.CreateGenerationContext()
        mkEmptyFuncCached<'T> ctx

and private mkEmptyFuncCached<'T> (ctx : TypeGenerationContext) : bool -> 'T =
    match ctx.InitOrGetCachedValue<bool -> 'T>(fun c m -> c.Value m) with
    | Cached(isValueCreated = false) -> failwithf "Type '%O' does not support empty values." typeof<'T>
    | Cached(value = f) -> f
    | NotCached t ->
        let f = mkEmptyFuncAux<'T> ctx
        ctx.Commit t f

and private mkEmptyFuncAux<'T> (ctx : TypeGenerationContext) : bool -> 'T =
    let EQ (f : bool -> 'a) = unbox<bool -> 'T> f

    let mkMemberInitializer (shape : IShapeMember<'DeclaringType>) =
        shape.Accept { new IMemberVisitor<'DeclaringType, bool -> 'DeclaringType -> 'DeclaringType> with
            member __.Visit (shape : ShapeMember<'DeclaringType, 'Field>) =
                let fe = mkEmptyFuncCached<'Field> ctx
                fun m inst -> shape.Set inst (fe m)
        }

    match shapeof<'T> with
    | Shape.Primitive -> fun _ -> Unchecked.defaultof<'T>
    | Shape.Decimal -> EQ(fun _ -> 0M)
    | Shape.BigInt -> EQ(fun _ -> 0I)
    | Shape.Guid -> EQ(fun _ -> Guid.Empty)
    | Shape.String -> EQ(fun _ -> "")
    | Shape.TimeSpan -> EQ(fun _ -> TimeSpan.Zero)
    | Shape.DateTime -> EQ(fun _ -> DateTime.MinValue)
    | Shape.DateTimeOffset -> EQ(fun _ -> DateTimeOffset.MinValue)
    | Shape.Unit -> EQ (fun _ -> ())
    | Shape.Enum s ->
        s.Accept { new IEnumVisitor<bool -> 'T> with
            member __.Visit<'t, 'u when 't : enum<'u>
                                    and 't : struct
                                    and 't :> ValueType
                                    and 't : (new : unit -> 't)>() = // 'T = 't
                let ue = mkEmptyFuncCached<'u> ctx
                EQ(fun m -> LanguagePrimitives.EnumOfValue<'u,'t>(ue m)) }

    | Shape.Nullable s ->
        s.Accept { new INullableVisitor<bool -> 'T> with
            member __.Visit<'t when 't : struct and 't :> ValueType and 't : (new : unit -> 't)>() = // 'T = Nullable<'t>
                let em = mkEmptyFuncCached<'t> ctx
                EQ(fun max -> if max then Nullable(em true) else Nullable()) }

    | Shape.FSharpFunc s ->
        // empty<'T -> 'S> = fun (_ : 'T) -> empty<'S>
        s.Accept { new IFSharpFuncVisitor<bool -> 'T> with
            member __.Visit<'Dom, 'Cod> () = // 'T = 'Cod -> 'Dom
                let de = mkEmptyFuncAux<'Cod> ctx
                EQ(fun max -> fun (_ : 'Dom) -> de max) }

    | Shape.FSharpOption s ->
        s.Element.Accept { new ITypeVisitor<bool -> 'T> with
            member __.Visit<'t>() = // 'T = 't option
                let et = mkEmptyFuncCached<'t> ctx
                EQ(fun max -> if max then Some (et max) else None) }

    | Shape.KeyValuePair s ->
        s.Accept { new IKeyValuePairVisitor<bool -> 'T> with
            member __.Visit<'k,'v>() = // 'T = KeyValuePair<'k,'v>
                let ke,ve = mkEmptyFuncCached<'k> ctx, mkEmptyFuncCached<'v> ctx
                EQ(fun max -> new KeyValuePair<'k,'v>(ke max,ve max)) }

    | Shape.FSharpList s ->
        s.Element.Accept { new ITypeVisitor<bool -> 'T> with
            member __.Visit<'t>() = // 'T = 't list
                let te = mkEmptyFuncCached<'t> ctx
                EQ(fun max -> if max then [te max] else []) } 

    | Shape.Array s ->
        s.Element.Accept { new ITypeVisitor<bool -> 'T> with
            member __.Visit<'t> () = 
                let te = mkEmptyFuncCached<'t> ctx
                let inline sz m = if m then 1 else 0
                match s.Rank with
                | 1 -> EQ(fun m -> let s = sz m in Array.init s (fun _ -> te m)) // 'T = 't []
                | 2 -> EQ(fun m -> let s = sz m in Array2D.init s s (fun _ _ -> te m)) // 'T = 't [,]
                | 3 -> EQ(fun m -> let s = sz m in Array3D.init s s s (fun _ _ _ -> te m)) // 'T = 't [,,]
                | 4 -> EQ(fun m -> let s = sz m in Array4D.init s s s s (fun _ _ _ _ -> te m)) // 'T = 't [,,,]
                | _ -> failwithf "Unsupported type %O" typeof<'T> }

    | Shape.FSharpSet s ->
        s.Accept { new IFSharpSetVisitor<bool -> 'T> with
            member __.Visit<'t when 't : comparison>() = // 'T = Set<'t>
                let te = mkEmptyFuncCached<'t> ctx
                EQ(fun max -> if max then set [|te max|] else Set.empty) } 

    | Shape.FSharpMap s ->
        s.Accept { new IFSharpMapVisitor<bool -> 'T> with
            member __.Visit<'k, 'v when 'k : comparison>() = // 'T = Map<'k,'v>
                let ke, ve = mkEmptyFuncCached<'k> ctx, mkEmptyFuncCached<'v> ctx
                fun max -> 
                    if max then Map.ofArray [|(ke max, ve max)|] 
                    else Map.empty
                |> EQ } 

    | Shape.Dictionary s ->
        s.Accept { new IDictionaryVisitor<bool -> 'T> with
            member __.Visit<'k, 'v when 'k : equality>() = // 'T = Dictionary<'k,'v>
                let kve = mkEmptyFuncCached<KeyValuePair<'k, 'v>> ctx
                fun max -> 
                    let d = new Dictionary<'k,'v>()
                    if max then let kv = kve max in d.Add(kv.Key, kv.Value)
                    d
                |> EQ }

    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
        let elemInitializers = shape.Elements |> Array.map mkMemberInitializer
        fun max ->
            let mutable inst = shape.CreateUninitialized()
            for f in elemInitializers do inst <- f max inst
            inst

    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
        let fieldInitializers = shape.Fields |> Array.map mkMemberInitializer
        fun max ->
            let mutable inst = shape.CreateUninitialized()
            for f in fieldInitializers do inst <- f max inst
            inst

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
        let defaultUnionCase = shape.UnionCases |> Array.minBy (fun c -> c.Fields.Length)
        let fieldInitializers = defaultUnionCase.Fields |> Array.map mkMemberInitializer
        fun max ->
            let mutable inst = defaultUnionCase.CreateUninitialized()
            for f in fieldInitializers do inst <- f max inst
            inst

    | Shape.CliMutable (:? ShapeCliMutable<'T> as shape) ->
        let propInitializers = shape.Properties |> Array.map mkMemberInitializer
        fun max -> 
            let mutable inst = shape.CreateUninitialized()
            if max then for p in propInitializers do inst <- p max inst
            inst

    | _ -> failwithf "Type '%O' does not support empty values." typeof<'T>

and private cache : TypeCache = new TypeCache()

/// Registers an empty factory for a given type
let register (emptyFactory : unit -> 'T) = cache.ForceAdd (fun (_:bool) -> emptyFactory())

/// Generates a structural empty value for given type
let empty<'T> = mkEmptyFunc<'T> () false

/// Generates a structural empty value that populates
/// variadic types with singletons
let notEmpty<'T> = mkEmptyFunc<'T> () true