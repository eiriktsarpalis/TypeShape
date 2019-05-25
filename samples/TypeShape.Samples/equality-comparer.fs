module EqualityComparer

open System
open System.Collections.Generic
open TypeShape.Core
open TypeShape.Core.Utils

// Generic Equality comparer generator

let rec mkEqualityComparer<'T> () : IEqualityComparer<'T> =
    let mutable f = Unchecked.defaultof<IEqualityComparer<'T>>
    if cache.TryGetValue(&f) then f
    else
        use mgr = cache.CreateGenerationContext()
        mkEqualityComparerCached<'T> mgr

and private mkEqualityComparerCached<'T> (ctx : TypeGenerationContext) : IEqualityComparer<'T> =
    let delay (c:Cell<IEqualityComparer<'T>>) =
        { new IEqualityComparer<'T> with
            member __.GetHashCode t = c.Value.GetHashCode t
            member __.Equals (t, t') = c.Value.Equals(t, t') }
    match ctx.InitOrGetCachedValue<IEqualityComparer<'T>> delay with
    | Cached(value = f) -> f
    | NotCached t ->
        let f = mkEqualityComparerAux<'T> ctx
        ctx.Commit t f

and private mkEqualityComparerAux<'T> (ctx : TypeGenerationContext) : IEqualityComparer<'T> =
    let inline combine (h1 : int) (h2 : int) = ((h1 <<< 5) + h1) ||| h2
    let inline wrap (hash : 'a -> int) (cmp : 'a -> 'a -> bool) =
        { new IEqualityComparer<'a> with
            member __.Equals(t,t') = cmp t t'
            member __.GetHashCode t = hash t } |> unbox<IEqualityComparer<'T>>

    let mkMembersComparer (members : #IShapeReadOnlyMember<'DeclaringType> []) =
        let mkMemberComparer (shape : IShapeReadOnlyMember<'DeclaringType>) =
            shape.Accept { new IReadOnlyMemberVisitor<'DeclaringType, IEqualityComparer<'DeclaringType>> with
                member __.Visit (shape : ReadOnlyMember<'DeclaringType, 'FieldType>) =
                    let fc = mkEqualityComparerCached<'FieldType> ctx
                    { new IEqualityComparer<'DeclaringType> with
                        member __.Equals(d1, d2) = fc.Equals (shape.Get d1, shape.Get d2)
                        member __.GetHashCode d = fc.GetHashCode (shape.Get d) }
            }

        let memberComps = members |> Array.map mkMemberComparer
        { new IEqualityComparer<'DeclaringType> with
            member __.Equals(d1, d2) = memberComps |> Array.forall (fun ec -> ec.Equals(d1,d2))
            member __.GetHashCode d =
                let mutable hash = 0
                for ec in memberComps do hash <- combine hash (ec.GetHashCode d)
                hash }

    match shapeof<'T> with
    | Shape.Unit -> wrap (fun () -> 0) (fun () () -> true)
    | Shape.Bool -> wrap (function false -> 0 | true -> 1) (=)
    | Shape.Byte -> wrap (fun (b:byte) -> int b) (=)
    | Shape.SByte -> wrap (fun (b:sbyte) -> int b) (=)
    | Shape.Int16 -> wrap (fun (i:int16) -> int i) (=)
    | Shape.Int32 -> wrap (fun (i:int) -> i) (=)
    | Shape.Int64 -> wrap (fun (i:int64) -> hash i) (=)
    | Shape.UInt16 -> wrap (fun (i:uint16) -> int i) (=)
    | Shape.UInt32 -> wrap (fun (i:uint32) -> hash i) (=)
    | Shape.UInt64 -> wrap (fun (i:uint64) -> hash i) (=)
    | Shape.BigInt -> wrap (fun (i:bigint) -> hash i) (=)
    | Shape.Single -> wrap (fun (f:single) -> hash f) (=)
    | Shape.Double -> wrap (fun (f:float) -> hash f) (=)
    | Shape.Decimal -> wrap (fun (f:decimal) -> hash f) (=)
    | Shape.DateTime -> wrap (fun (f:DateTime) -> hash f) (=)
    | Shape.Char -> wrap (fun (c:Char) -> hash c) (=)
    | Shape.TimeSpan -> wrap (fun (f:TimeSpan) -> hash f) (=)
    | Shape.String -> wrap (fun (s:string) -> s.GetHashCode()) (=)
    | Shape.Enum s ->
        s.Accept { 
            new IEnumVisitor<IEqualityComparer<'T>> with
                member __.Visit<'e, 'u when 'e : enum<'u>
                                        and 'e : struct
                                        and 'e :> ValueType
                                        and 'e : (new : unit -> 'e)>() =

                    let uc = mkEqualityComparerCached<'u> ctx
                    wrap (fun (e:'e) -> uc.GetHashCode(LanguagePrimitives.EnumToValue e))
                         (fun (e1:'e) (e2:'e) -> uc.Equals(LanguagePrimitives.EnumToValue e1, LanguagePrimitives.EnumToValue e2)) }

    | Shape.FSharpOption s ->
        s.Element.Accept {
            new ITypeVisitor<IEqualityComparer<'T>> with
                member __.Visit<'a> () =
                    let tc = mkEqualityComparerCached<'a> ctx
                    wrap (function None -> 0 | Some t -> tc.GetHashCode t)
                        (fun xo yo -> 
                            match xo with
                            | None -> match yo with None -> true | _ -> false
                            | Some x -> match yo with None -> false | Some y -> tc.Equals(x,y))
        }

    | Shape.FSharpList s ->
        s.Element.Accept {
            new ITypeVisitor<IEqualityComparer<'T>> with
                member __.Visit<'a> () =
                    let tc = mkEqualityComparerCached<'a> ctx
                    let rec hash c (xs : 'a list) =
                        match xs with
                        | [] -> c
                        | x :: xs' -> hash (combine c (tc.GetHashCode x)) xs'
                        
                    let rec equals (xs : 'a list) (ys : 'a list) =
                        match xs with
                        | [] -> match ys with [] -> true | _ -> false
                        | x :: xs' -> 
                            match ys with
                            | y :: ys' when tc.Equals(x,y) -> equals xs' ys'
                            | _ -> false

                    wrap (hash 0) equals
        }

    | Shape.Array s when s.Rank = 1 ->
        s.Element.Accept {
            new ITypeVisitor<IEqualityComparer<'T>> with
                member __.Visit<'a> () =
                    let tc = mkEqualityComparerCached<'a> ctx
                    let hash (xs : 'a []) =
                        let mutable h = 0
                        for x in xs do h <- combine h (tc.GetHashCode x)
                        h
                        
                    let equals (xs : 'a []) (ys : 'a []) =
                        if obj.ReferenceEquals(xs,null) then obj.ReferenceEquals(ys,null)
                        elif xs.Length <> ys.Length then false
                        else
                            let rec aux i n =
                                if i = n then true
                                elif tc.Equals(xs.[i], ys.[i]) then aux (i + 1) n
                                else false

                            aux 0 xs.Length

                    wrap hash equals    
        }

    | Shape.KeyValuePair s ->
        s.Accept {
            new IKeyValuePairVisitor<IEqualityComparer<'T>> with
                member __.Visit<'k, 'v> () =
                    let kc = mkEqualityComparerCached<'k> ctx
                    let vc = mkEqualityComparerCached<'v> ctx
                    let hash (kv : KeyValuePair<'k,'v>) =
                        combine (kc.GetHashCode kv.Key) (vc.GetHashCode kv.Value)

                    let equals (kv1 : KeyValuePair<'k,'v>) (kv2 : KeyValuePair<'k,'v>) =
                        kc.Equals(kv1.Key, kv2.Key) && vc.Equals(kv1.Value, kv2.Value)

                    wrap hash equals
        }

    | Shape.FSharpSet s ->
        s.Accept {
            new IFSharpSetVisitor<IEqualityComparer<'T>> with
                member __.Visit<'T when 'T : comparison> () =
                    let tc = mkEqualityComparerCached<'T> ctx
                    let hash (s : Set<'T>) =
                        let mutable h = 0
                        for e in s do h <- combine h (tc.GetHashCode e)
                        h

                    let equals (s : Set<'T>) (s' : Set<'T>) =
                        if s.Count <> s'.Count then false
                        else
                            use e1 = (s :> seq<'T>).GetEnumerator()
                            use e2 = (s' :> seq<'T>).GetEnumerator()
                            let rec aux () =
                                if not(e1.MoveNext() && e2.MoveNext()) then true
                                elif tc.Equals(e1.Current, e2.Current) then aux ()
                                else false

                            aux ()

                    wrap hash equals
        }

    | Shape.FSharpMap s ->
        s.Accept {
            new IFSharpMapVisitor<IEqualityComparer<'T>> with
                member __.Visit<'k, 'v when 'k : comparison> () =
                    let kvc = mkEqualityComparerCached<KeyValuePair<'k,'v>> ctx

                    let hash (s : Map<'k,'v>) =
                        let mutable h = 0
                        for kv in s do 
                            h <- combine h (kvc.GetHashCode kv)
                        h

                    let equals (s : Map<'k,'v>) (s' : Map<'k,'v>) =
                        if s.Count <> s'.Count then false else

                        use e1 = (s :> seq<KeyValuePair<'k,'v>>).GetEnumerator()
                        use e2 = (s' :> seq<KeyValuePair<'k,'v>>).GetEnumerator()
                        let rec aux () =
                            if not(e1.MoveNext() && e2.MoveNext()) then true
                            elif kvc.Equals(e1.Current, e2.Current) then aux ()
                            else false

                        aux ()

                    wrap hash equals
        }

    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
        mkMembersComparer shape.Elements

    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
        mkMembersComparer shape.Fields

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
        let unionCaseComparers = shape.UnionCases |> Array.map (fun uc -> mkMembersComparer uc.Fields)
        { new IEqualityComparer<'T> with
            member __.Equals (u1,u2) =
                match shape.GetTag u1, shape.GetTag u2 with
                | t1, t2 when t1 <> t2 -> false
                | tag, _ -> unionCaseComparers.[tag].Equals(u1,u2)

            member __.GetHashCode u = 
                let tag = shape.GetTag u
                combine tag (unionCaseComparers.[tag].GetHashCode u) }

    | _ -> failwithf "unsupported type '%O'" typeof<'T>


and private cache : TypeCache = new TypeCache()

let comparer<'T> = mkEqualityComparer<'T>()


let c0 = comparer<int list>

let value = [1 .. 1000000]
c0.GetHashCode value
c0.Equals(value,value)
// value.GetHashCode() // stack overflow error