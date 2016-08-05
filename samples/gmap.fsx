#load "../src/TypeShape/TypeShape.fs"
open System
open TypeShape

// adapted from http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/gmap3.pdf

let rec gmapQ<'T,'S,'U> (f : 'T -> 'S) : 'U -> 'S list = gmapQUntyped f typeof<'U> :?> _
and private gmapQUntyped (f : 'T -> 'S) (t : Type) : obj =
    let wrap(f : 'a -> 'S list) = box f
    match TypeShape.Create t with
    | :? TypeShape<'T> -> wrap(fun (t:'T) -> [f t])
    | Shape.FSharpOption s ->
        s.Accept {
            new IFSharpOptionVisitor<obj> with
                member __.Visit<'a>() =
                    let tm = gmapQ<'T,'S,'a> f
                    wrap(function None -> [] | Some t -> tm t)
        }

    | Shape.Tuple2 s ->
        s.Accept {
            new ITuple2Visitor<obj> with
                member __.Visit<'a,'b>() =
                    let am = gmapQ<'T,'S,'a> f
                    let bm = gmapQ<'T,'S,'b> f
                    wrap(fun (a,b) -> am a @ bm b)
        }

    | Shape.Tuple3 s ->
        s.Accept {
            new ITuple3Visitor<obj> with
                member __.Visit<'a,'b,'c>() =
                    let am = gmapQ<'T,'S,'a> f
                    let bm = gmapQ<'T,'S,'b> f
                    let cm = gmapQ<'T,'S,'c> f
                    wrap(fun (a,b,c) -> am a @ bm b @ cm c)
        }

    | Shape.FSharpList s ->
        s.Accept {
            new IFSharpListVisitor<obj> with
                member __.Visit<'a>() =
                    let tm = gmapQ<'T,'S,'a> f
                    wrap(fun (ts : 'a list) -> ts |> List.collect tm)
        }

    | Shape.FSharpSet s ->
        s.Accept {
            new IFSharpSetVisitor<obj> with
                member __.Visit<'a when 'a : comparison> () =
                    let tm = gmapQ<'T,'S,'a> f
                    wrap(fun (ts : Set<'a>) -> ts |> Seq.collect tm |> Seq.toList)
        }

    | s -> 
        s.Accept { new ITypeShapeVisitor<obj> with
            member __.Visit<'a>() = wrap(fun (_:'a) -> []) }

gmapQ id<int> (Some 42, ([1 .. 10], set[5;6;7]))
gmapQ id<string> (Some 42, ([1 .. 10], "42", set[5;6;7]))