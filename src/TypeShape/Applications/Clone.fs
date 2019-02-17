module TypeShape.Clone

open System
open System.Runtime.Serialization
open TypeShape.Core
open TypeShape.Core.Utils

module private Impl =

    type Cloner<'T> = ObjectStack -> ObjectCache -> 'T -> 'T

    let rec mkCloner<'T> () : Cloner<'T> =
        let mutable f = Unchecked.defaultof<Cloner<'T>>
        if cache.TryGetValue<Cloner<'T>>(&f) then f else
        use ctx = cache.CreateGenerationContext()
        mkClonerCached<'T> ctx

    and cloneUntyped s c (o:obj) : obj =
        if obj.ReferenceEquals(o,null) then null else
        let shape = o.GetType() |> TypeShape.Create
        shape.Accept { new ITypeShapeVisitor<obj> with
            member __.Visit<'T>() = mkCloner () s c (o :?> 'T) :> obj }

    and mkRefEqCloner<'T> (cloner : Cloner<'T>) : Cloner<'T> =
        match shapeof<'T> with
        | Shape.Nullable _
        | Shape.Struct _ -> cloner
        | Shape.NotStruct s ->
            s.Accept { new INotStructVisitor<Cloner<'T>> with
                member __.Visit<'t when 't : not struct and 't : null>() =
                    cloner
                    |> unbox<Cloner<'t>> 
                    |> mkRefEqClonerAux
                    |> unbox<Cloner<'T>> }

    and mkRefEqClonerAux<'T when 'T : not struct> (cloner : Cloner<'T>) : Cloner<'T> =
        let isOpen =
            match shapeof<'T> with
            | Shape.FSharpUnion _ -> false
            | _ -> not typeof<'T>.IsSealed

        fun stack cache t ->
            if obj.ReferenceEquals(t,null) then t 
            elif isOpen && t.GetType() <> typeof<'T> then
                cloneUntyped stack cache t :?> 'T
            else
                do stack.Push t
                let id = stack.ObjectId
                let t' =
                    let mutable t' = Unchecked.defaultof<_>
                    if cache.TryGetValue(id, &t') then t'
                    elif stack.IsCycle then
                        cache.CreateUninitializedInstance<'T>(id, t.GetType())
                    else
                        let t' = cloner stack cache t
                        cache.AddValue(id, t')

                do stack.Pop()
                t'

    and mkClonerCached<'T> (ctx : TypeGenerationContext) : Cloner<'T> =
        match ctx.InitOrGetCachedValue<Cloner<'T>> (fun c s t -> c.Value s t) with
        | Cached(value = v) -> v
        | NotCached t ->
            let c = mkClonerMain<'T> ctx |> mkRefEqCloner
            ctx.Commit t c

    and mkClonerMain<'T> (ctx : TypeGenerationContext) : Cloner<'T> =
        let EQ (c : Cloner<'a>) : Cloner<'T> = unbox c

        let mkMemberCloner (fieldShape : IShapeWriteMember<'DeclaringType>) =
            fieldShape.Accept {
                new IWriteMemberVisitor<'DeclaringType, ObjectStack -> ObjectCache -> 'DeclaringType -> 'DeclaringType -> 'DeclaringType> with
                    member __.Visit (shape : ShapeWriteMember<'DeclaringType, 'Field>) =
                        let fieldCloner = mkClonerCached<'FieldType> ctx
                        fun s c src tgt ->
                            let field = shape.Project src
                            let field' = fieldCloner s c field
                            shape.Inject tgt field'
            }

        match shapeof<'T> with
        | Shape.Primitive
        | Shape.TimeSpan
        | Shape.DateTimeOffset
        | Shape.DateTime
        | Shape.BigInt
        | Shape.Unit
        | Shape.Decimal
        | Shape.Enum _ -> fun _ _ x -> x
        | Shape.String -> EQ(fun _ _ s -> String.Copy(s))
        | Shape.Array s when s.Rank = 1 ->
            s.Accept {
                new IArrayVisitor<Cloner<'T>> with
                    member __.Visit<'t> _ =
                        if typeof<'t>.IsPrimitive then
                            EQ(fun _ _ (ts:'t[]) -> ts.Clone() :?> 't[])
                        else
                            let ec = mkClonerCached<'t> ctx
                            EQ(fun s c (ts:'t[]) ->
                                // pre-register the uninitialized array
                                // to account for cylic arrays
                                let ts' = Array.zeroCreate<'t> ts.Length
                                let _ = c.AddValue(s.ObjectId, ts')
                                for i = 0 to ts.Length - 1 do ts'.[i] <- ec s c ts.[i]
                                ts') }

        | Shape.Nullable s ->
            s.Accept { new INullableVisitor<Cloner<'T>> with
                member __.Visit<'t when 't : (new : unit -> 't) 
                                    and 't :> ValueType 
                                    and 't : struct> () =

                    let ec = mkClonerCached<'t> ctx
                    EQ(fun s c (t:Nullable<'t>) -> 
                        if t.HasValue then Nullable(ec s c t.Value)
                        else t) }

        | Shape.FSharpList s ->
            s.Accept {
                new IFSharpListVisitor<Cloner<'T>> with
                    member __.Visit<'t> () =
                        let ec = mkClonerCached<'t> ctx
                        EQ(fun s c ts -> List.map (ec s c) ts) }

        | Shape.FSharpSet s ->
            s.Accept {
                new IFSharpSetVisitor<Cloner<'T>> with
                    member __.Visit<'t when 't : comparison> () =
                        let tc = mkClonerCached<'t> ctx
                        EQ(fun s c ts -> Set.map (tc s c) ts) }

        | Shape.FSharpMap s ->
            s.Accept {
                new IFSharpMapVisitor<Cloner<'T>> with
                    member __.Visit<'k, 'v when 'k : comparison> () =
                        let kc = mkClonerCached<'k> ctx
                        let vc = mkClonerCached<'v> ctx
                        EQ(fun s c (ts:Map<'k,'v>) -> ts |> Seq.map (fun kv -> kc s c kv.Key, vc s c kv.Value) |> Map.ofSeq) }

        | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
            let memberCloners = shape.Elements |> Array.map mkMemberCloner
            fun s c source ->
                let mutable target = shape.CreateUninitialized()
                for mc in memberCloners do
                    target <- mc s c source target
                target

        | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
            let memberCloners = shape.Fields |> Array.map mkMemberCloner
            fun s c source ->
                let mutable target = shape.CreateUninitialized()
                for mc in memberCloners do
                    target <- mc s c source target
                target

        | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
            let caseMemberCloners = 
                shape.UnionCases 
                |> Array.map (fun c -> c.Fields |> Array.map mkMemberCloner)

            fun s c source ->
                let tag = shape.GetTag source
                let case = shape.UnionCases.[tag]
                let memberCloners = caseMemberCloners.[tag]
                let mutable target = case.CreateUninitialized()
                for mc in memberCloners do
                    target <- mc s c source target
                target

        | Shape.ISerializable s ->
            s.Accept { new ISerializableVisitor<Cloner<'T>> with
                member __.Visit (shape : ShapeISerializable<'S>) =
                    fun s c (source : 'S) ->
                        let sc = new StreamingContext()
                        let si = new SerializationInfo(typeof<'S>, FormatterConverter())
                        let si' = new SerializationInfo(typeof<'S>, FormatterConverter())

                        source.GetObjectData(si, sc)
                        let e = si.GetEnumerator()
                        while e.MoveNext() do si'.AddValue(e.Name, cloneUntyped s c e.Value)

                        shape.Create(si, sc)
                    |> EQ }

        | Shape.CliMutable (:? ShapeCliMutable<'T> as shape) ->
            let memberCloners = shape.Properties |> Array.map mkMemberCloner
            fun s c source ->
                let mutable target = shape.CreateUninitialized()
                for mc in memberCloners do
                    target <- mc s c source target
                target

        | Shape.Poco (:? ShapePoco<'T> as shape) ->
            let fieldCloners = shape.Fields |> Array.map mkMemberCloner
            fun s c source ->
                let mutable target = shape.CreateUninitialized()
                for fc in fieldCloners do
                    target <- fc s c source target
                target

        | _ -> failwithf "Unsupported type %O" typeof<'T>

    and cache : TypeCache = new TypeCache()


/// Creates a deep clone for the provided value.
/// Accounts for reference equality and object cycles.
let clone<'T> (t : 'T) : 'T = Impl.mkCloner<'T>() (ObjectStack()) (ObjectCache()) t