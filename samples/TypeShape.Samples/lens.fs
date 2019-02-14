module Lens

open FSharp.Quotations
open FSharp.Quotations.Patterns
open TypeShape.Core
open TypeShape.Clone

type Lens<'T, 'F> =
    {
        get : 'T -> 'F
        set : 'T -> 'F -> 'T
    }

module private Impl =

    type Path = Node list
    and Node =
        | Property of string
        | Item of int

    // converts a quotation of the form <@ fun x -> x.Foo.Bar.[0].Baz @> into a path
    let extractPath (e : Expr<'T -> 'F>) : Path =
        let rec aux v acc e =
            match e with
            | Var v' when v = v' -> acc
            | PropertyGet(Some o, p, [Value((:? int as i), _)]) when p.Name = "Item" -> aux v (Item i :: acc) o
            | PropertyGet(Some o, p, []) -> aux v (Property p.Name :: acc) o
            | Call(None, m, [o ; Value(:? int as i, _)]) when m.Name = "GetArray" && o.Type.IsArray && e.Type = o.Type.GetElementType() -> aux v (Item i :: acc) o
            // we support tuples, as they are often used to encode fields in erased type providers
            | TupleGet(x, i) -> aux v (Item i :: acc) x
            | _ -> invalidArg "expr" "invalid lens expression"

        match e with
        | Lambda(v, body) -> aux v [] body
        | _ -> invalidArg "expr" "lens expressions must be lambda literals"

    let rec mkLensAux<'T, 'F> (path : Path) : Lens<'T, 'F> =
        let wrap (l : Lens<'a,'b>) : Lens<'T, 'F> = unbox l

        let nest chain (m : IShapeWriteMember<'T>) =
            m.Accept { new IWriteMemberVisitor<'T, Lens<'T, 'F>> with
                member __.Visit<'F0> (m : ShapeWriteMember<'T, 'F0>) =
                    let inner = mkLensAux<'F0, 'F> chain
                    {
                        get = fun (t:'T) -> inner.get (m.Project t)
                        set = fun (t:'T) (f:'F) -> m.Inject t (inner.set (m.Project t) f)
                    }
        
            }

        match shapeof<'T>, path with
        | _, [] -> wrap { get = id<'F> ; set = fun (_:'F) (y:'F) -> y }
        | Shape.FSharpList s, Item i :: rest ->
            s.Accept { new IFSharpListVisitor<Lens<'T,'F>> with
                member __.Visit<'t> () =
                    let inner = mkLensAux<'t, 'F> rest
                    wrap {
                        get = fun (ts : 't list) -> inner.get ts.[i]
                        set = fun (ts : 't list) (f : 'F) -> ts |> List.mapi (fun j t -> if j = i then inner.set t f else t)
                    }
            }

        | Shape.FSharpOption s, Property "Value" :: rest ->
            s.Accept { new IFSharpOptionVisitor<Lens<'T,'F>> with
                member __.Visit<'t> () =
                    let inner = mkLensAux<'t, 'F> rest
                    wrap {
                        get = fun (ts : 't option) -> inner.get (Option.get ts)
                        set = fun (ts : 't option) (f : 'F) -> inner.set (Option.get ts) f |> Some
                    }
            }

        | Shape.Tuple (:? ShapeTuple<'T> as s), Item i :: rest ->
            s.Elements.[i] |> nest rest

        | Shape.Array s, Item i :: rest when s.Rank = 1 ->
            s.Accept { new IArrayVisitor<Lens<'T,'F>> with
                member __.Visit<'t> _ =
                    let inner = mkLensAux<'t, 'F> rest
                    wrap {
                        get = fun (ts : 't[]) -> inner.get ts.[i]
                        set = fun (ts : 't[]) (f : 'F) ->  ts.[i] <- inner.set ts.[i] f ; ts
                    }
            }

        | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as s) & Shape.FSharpRef _, Property "Value" :: rest ->
            s.Fields |> Array.find (fun p -> p.Label = "contents") |> nest rest

        | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as s), Property id :: rest ->
            s.Fields |> Array.find (fun p -> p.Label = id) |> nest rest

        | _ -> failwithf "unsupported lens type %O" typeof<'T>


let mkLens<'T, 'F> (expr : Expr<'T -> 'F>) : Lens<'T, 'F> =
    let path = Impl.extractPath expr
    let lens = Impl.mkLensAux<'T, 'F> path
    {
        get = lens.get
        set = fun t f -> lens.set (clone t) f // TypeShape native lenses rely on mutation, so we clone to hide this fact
    }


//--------------------------------------------
// Examples


type Foo<'T> =
    {
        a : 'T
        b : string
        c : bool
    }


let l1 = mkLens <@ fun (x:int) -> x @>
let l2 = mkLens <@ fun (x:Foo<int> ref) -> x.Value.a @>
let l3 = mkLens <@ fun (x:Foo<Foo<int> list> [] list option) -> x.Value.[0].[0].a.[0] @>

l1.get 42 // 42
l2.get (ref {a = 42; b = "" ; c = false}) // 42
l3.get (Some [[| { a = [{ a = 42 ; b = "" ; c = false }] ; b = "" ; c = false } |]]) // { a = 42 ; b = "" ; c = false }

l1.set 42 1
l2.set (ref {a = 42; b = "" ; c = false}) 5
l3.set (Some [[| { a = [{ a = 42 ; b = "" ; c = false }] ; b = "" ; c = false } |]]) { a = 42 ; b = "b" ; c = true }

// working with type providers

open FSharp.Data
type Bar = CsvProvider<Schema = "A (int), B (string), C (float)", HasHeaders = false>

let l4 = mkLens <@ fun (x:Bar.Row list) -> x.[1].B @>

let values = Bar.Parse("42, bar, 3.14\n55, baz, 2.17").Rows |> Seq.toList

l4.get values
l4.set values "foo"