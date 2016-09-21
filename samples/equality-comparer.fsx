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

    | Shape.Array s ->
        s.Accept {
            new IArrayVisitor<IEqualityComparer<'T>> with
                member __.Visit<'a> () =
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

    | Shape.Tuple2 s ->
        s.Accept {
            new ITuple2Visitor<IEqualityComparer<'T>> with
                member __.Visit<'t1, 't2> () =
                    let tc = mkEqualityComparer<'t1>()
                    let sc = mkEqualityComparer<'t2>()
                    let hash (t : 't1, s : 't2) = combine (tc.GetHashCode t) (sc.GetHashCode s)
                    let equals (t : 't1, s : 't2) (t' : 't1, s' : 't2) = tc.Equals(t,t') && sc.Equals(s, s')
                    wrap hash equals
        }

    | Shape.Tuple3 s ->
        s.Accept {
            new ITuple3Visitor<IEqualityComparer<'T>> with
                member __.Visit<'t1, 't2, 't3> () =
                    let t1c = mkEqualityComparer<'t1>()
                    let t2c = mkEqualityComparer<'t2>()
                    let t3c = mkEqualityComparer<'t3>()
                    let hash (t1 : 't1, t2 : 't2, t3 : 't3) =
                        combine (combine (t1c.GetHashCode t1) (t2c.GetHashCode t2)) (t3c.GetHashCode t3)
                    let equals (t1 : 't1, t2 : 't2, t3 : 't3) (t1' : 't1, t2' : 't2, t3' : 't3) =
                        t1c.Equals(t1, t1') && t2c.Equals(t2, t2') && t3c.Equals(t3, t3')

                    wrap hash equals
        }

    | Shape.FSharpRecord1 s ->
        s.Accept {
            new IFSharpRecord1Visitor<IEqualityComparer<'T>> with
                member __.Visit (s : IShapeFSharpRecord<'Record, 'Field1>) =
                    let f1c = mkEqualityComparer<'Field1>()
                    wrap (s.Project1 >> f1c.GetHashCode) 
                        (fun (r : 'Record) (r' : 'Record) -> f1c.Equals (s.Project1 r, s.Project1 r'))
        }

    | Shape.FSharpRecord2 s ->
        s.Accept {
            new IFSharpRecord2Visitor<IEqualityComparer<'T>> with
                member __.Visit<'Record,'Field1,'Field2> (s : IShapeFSharpRecord<'Record,'Field1,'Field2>) =
                    let f1c,f2c = mkEqualityComparer<'Field1>(), mkEqualityComparer<'Field2>()
                    let hash (r : 'Record) = combine (f1c.GetHashCode (s.Project1 r)) (f2c.GetHashCode (s.Project2 r))
                    let equals (r : 'Record) (r' : 'Record) = f1c.Equals(s.Project1 r, s.Project1 r') && f2c.Equals(s.Project2 r, s.Project2 r')
                    wrap hash equals
        }

    | Shape.FSharpUnion1 s ->
        s.Accept {
            new IFSharpUnion1Visitor<IEqualityComparer<'T>> with
                member __.Visit (s : IShapeFSharpUnion<'Union,'Case1>) =
                    let c1c = mkEqualityComparer<'Case1>()
                    let hash (u : 'Union) = c1c.GetHashCode(s.Project u)
                    let equals (u : 'Union) (u' : 'Union) = c1c.Equals(s.Project u, s.Project u')
                    wrap hash equals
        }

    | Shape.FSharpUnion2 s ->
        s.Accept {
            new IFSharpUnion2Visitor<IEqualityComparer<'T>> with
                member __.Visit<'Union,'Case1,'Case2> (s : IShapeFSharpUnion<'Union,'Case1,'Case2>) =
                    let c1c, c2c = mkEqualityComparer<'Case1>(), mkEqualityComparer<'Case2>()
                    let hash (u : 'Union) =
                        match s.Project u with
                        | Choice1Of2 c1 -> combine 0 (c1c.GetHashCode c1)
                        | Choice2Of2 c2 -> combine 1 (c2c.GetHashCode c2)

                    let equals (u : 'Union) (u' : 'Union) =
                        match s.Project u with
                        | Choice1Of2 c1 -> match s.Project u' with Choice1Of2 c1' -> c1c.Equals(c1,c1') | _ -> false
                        | Choice2Of2 c2 -> match s.Project u' with Choice2Of2 c2' -> c2c.Equals(c2,c2') | _ -> false

                    wrap hash equals
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