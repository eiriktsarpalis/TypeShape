#r "../bin/TypeShape.dll"

open System
open System.Collections.Concurrent
open System.Collections.Generic
open TypeShape

let rec mkEqualityComparer<'T> () : IEqualityComparer<'T> =
    let inline combine (h1 : int) (h2 : int) = ((h1 <<< 5) + h1) ||| h2
    let inline wrap (hash : 'a -> int) (cmp : 'a -> 'a -> bool) =
        { new IEqualityComparer<'a> with
            member __.Equals(t,t') = cmp t t'
            member __.GetHashCode t = hash t } |> unbox<IEqualityComparer<'T>>

    let mkMembersComparer (members : #IShapeMember<'DeclaringType> []) =
        let mkMemberComparer (shape : IShapeMember<'DeclaringType>) =
            shape.Accept { new IMemberVisitor<'DeclaringType, IEqualityComparer<'DeclaringType>> with
                member __.Visit (shape : ShapeMember<'DeclaringType, 'FieldType>) =
                    let fc = mkEqualityComparer<'FieldType>()
                    { new IEqualityComparer<'DeclaringType> with
                        member __.Equals(d1, d2) = fc.Equals (shape.Project d1, shape.Project d2)
                        member __.GetHashCode d = fc.GetHashCode (shape.Project d) }
            }

        let memberComps = members |> Array.map mkMemberComparer
        { new IEqualityComparer<'DeclaringType> with
            member __.Equals(d1, d2) = memberComps |> Array.forall (fun ec -> ec.Equals(d1,d2))
            member __.GetHashCode d =
                let mutable hash = 0
                for ec in memberComps do hash <- combine hash (ec.GetHashCode d)
                hash }

    match TypeShape.Create<'T>() with
    | Shape.Unit -> wrap (fun () -> 0) (fun () () -> true)
    | Shape.Bool -> wrap (function false -> 0 | true -> 1) (=)
    | Shape.Byte -> wrap (fun (b:byte) -> int b) (=)
    | Shape.Int32 -> wrap id (=)
    | Shape.Int64 -> wrap (fun (i:int64) -> int i) (=)
    | Shape.Double -> wrap (fun (f:float) -> hash f) (=)
    | Shape.Decimal -> wrap (fun (f:decimal) -> hash f) (=)
    | Shape.String -> wrap (fun (s:string) -> s.GetHashCode()) (=)
    | Shape.FSharpOption s ->
        s.Accept {
            new IFSharpOptionVisitor<IEqualityComparer<'T>> with
                member __.Visit<'a> () =
                    let tc = mkEqualityComparer<'a>()
                    wrap (function None -> 0 | Some t -> tc.GetHashCode t)
                        (fun xo yo -> 
                            match xo with
                            | None -> match yo with None -> true | _ -> false
                            | Some x -> match yo with None -> false | Some y -> tc.Equals(x,y))
        }

    | Shape.FSharpList s ->
        s.Accept {
            new IFSharpListVisitor<IEqualityComparer<'T>> with
                member __.Visit<'a> () =
                    let tc = mkEqualityComparer<'a>()
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
        s.Accept {
            new IArrayVisitor<IEqualityComparer<'T>> with
                member __.Visit<'a> _ =
                    let tc = mkEqualityComparer<'a>()
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

    | Shape.Tuple s ->
        s.Accept {
            new ITupleVisitor<IEqualityComparer<'T>> with
                member __.Visit (shape : ShapeTuple<'Tuple>) =
                    mkMembersComparer shape.Elements |> unbox
        }

    | Shape.FSharpRecord s ->
        s.Accept {
            new IFSharpRecordVisitor<IEqualityComparer<'T>> with
                member __.Visit (shape : ShapeFSharpRecord<'Record>) =
                    mkMembersComparer shape.Fields |> unbox
        }

    | Shape.FSharpUnion s ->
        s.Accept {
            new IFSharpUnionVisitor<IEqualityComparer<'T>> with
                member __.Visit (shape : ShapeFSharpUnion<'Union>) =
                    let unionCaseComparers = shape.UnionCases |> Array.map (fun uc -> mkMembersComparer uc.Fields)
                    { new IEqualityComparer<'Union> with
                        member __.Equals (u1,u2) =
                            match shape.GetTag u1, shape.GetTag u2 with
                            | t1, t2 when t1 <> t2 -> false
                            | tag, _ -> unionCaseComparers.[tag].Equals(u1,u2)

                        member __.GetHashCode u = 
                            let tag = shape.GetTag u
                            combine tag (unionCaseComparers.[tag].GetHashCode u) } |> unbox
        }

    | Shape.FSharpSet s ->
        s.Accept {
            new IFSharpSetVisitor<IEqualityComparer<'T>> with
                member __.Visit<'T when 'T : comparison> () =
                    let tc = mkEqualityComparer<'T> ()
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

    | _ -> failwithf "unsupported type '%O'" typeof<'T>


let private cache = new ConcurrentDictionary<Type,obj>()
let comparer<'T> = cache.GetOrAdd(typeof<'T>, fun _ -> mkEqualityComparer<'T>() :> obj) :?> IEqualityComparer<'T>


let c0 = comparer<int list>

let value = [1 .. 1000000]
c0.GetHashCode value
c0.Equals(value,value)
// value.GetHashCode() // stack overflow error