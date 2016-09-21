#r "../bin/TypeShape.dll"
open System
open TypeShape

// inspired by but different from http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/gmap3.pdf

let rec gmapQ<'T,'S,'U> (f : 'T -> 'S) : 'U -> 'S list =
    let wrap(f : 'a -> 'S list) = unbox<'U -> 'S list> f
    match TypeShape.Create<'U>() :> TypeShape with
    | :? TypeShape<'T> -> wrap(fun (t:'T) -> [f t])
    | Shape.FSharpOption s ->
        s.Accept {
            new IFSharpOptionVisitor<'U -> 'S list> with
                member __.Visit<'a>() =
                    let tm = gmapQ<'T,'S,'a> f
                    wrap(function None -> [] | Some t -> tm t)
        }

    | Shape.Tuple2 s ->
        s.Accept {
            new ITuple2Visitor<'U -> 'S list> with
                member __.Visit<'a,'b>() =
                    let am = gmapQ<'T,'S,'a> f
                    let bm = gmapQ<'T,'S,'b> f
                    wrap(fun (a,b) -> am a @ bm b)
        }

    | Shape.Tuple3 s ->
        s.Accept {
            new ITuple3Visitor<'U -> 'S list> with
                member __.Visit<'a,'b,'c>() =
                    let am = gmapQ<'T,'S,'a> f
                    let bm = gmapQ<'T,'S,'b> f
                    let cm = gmapQ<'T,'S,'c> f
                    wrap(fun (a,b,c) -> am a @ bm b @ cm c)
        }

    | Shape.FSharpList s ->
        s.Accept {
            new IFSharpListVisitor<'U -> 'S list> with
                member __.Visit<'a>() =
                    let tm = gmapQ<'T,'S,'a> f
                    wrap(fun (ts : 'a list) -> ts |> List.collect tm)
        }

    | Shape.FSharpSet s ->
        s.Accept {
            new IFSharpSetVisitor<'U -> 'S list> with
                member __.Visit<'a when 'a : comparison> () =
                    let tm = gmapQ<'T,'S,'a> f
                    wrap(fun (ts : Set<'a>) -> ts |> Seq.collect tm |> Seq.toList)
        }

    | s -> 
        s.Accept { new ITypeShapeVisitor<'U -> 'S list> with
            member __.Visit<'a>() = wrap(fun (_:'a) -> []) }

gmapQ id<int> (Some 42, ([1 .. 10], set[5;6;7]))
gmapQ id<string> (Some 42, ([1 .. 10], "42", set[5;6;7]))