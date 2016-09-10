#load "../src/TypeShape/TypeShape.fs"

open System
open System.Reflection
open TypeShape

// Poor man's random value generator

type Gen<'T> = Random -> 'T

type Random with
    member r.NextBool() = r.Next(2) = 0

let rec mkGenerator<'T> () : Gen<'T> =
    let wrap (t : Gen<'a>) = unbox<Gen<'T>> t
    match TypeShape.Create<'T>() with
    | Shape.Unit -> wrap ignore
    | Shape.Byte -> wrap(fun r -> r.Next() |> byte)
    | Shape.Bool -> wrap(fun r -> r.NextBool())
    | Shape.Int32 -> wrap(fun r -> r.Next())
    | Shape.Int64 -> wrap(fun r -> let l,h = int64 (r.Next()), int64 (r.Next()) in l ||| (h <<< 32))
    | Shape.Double -> wrap(fun r -> r.NextDouble())
    | Shape.Char -> 
        let L = int Char.MinValue
        let U = int Char.MaxValue - L
        wrap(fun r -> r.Next(U + 1) + L |> char)

    | Shape.String ->
        let cGen = mkGenerator<char []>()
        wrap(fun r -> cGen r |> String)

    | Shape.Guid ->
        wrap(fun r -> let bytes = Array.zeroCreate<byte> 16 in r.NextBytes(bytes) ; new Guid(bytes))

    | Shape.FSharpOption s ->
        s.Accept {
            new IFSharpOptionVisitor<Gen<'T>> with
                member __.Visit<'t> () =
                    let tGen = mkGenerator<'t>()
                    wrap(fun r -> if r.NextBool() then None else Some(tGen r))
        }

    | Shape.Tuple2 s ->
        s.Accept {
            new ITuple2Visitor<Gen<'T>> with
                member __.Visit<'t1,'t2> () =
                    let t1G,t2G = mkGenerator<'t1>(), mkGenerator<'t2>()
                    wrap(fun r -> t1G r, t2G r)
        }

    | Shape.Tuple3 s ->
        s.Accept {
            new ITuple3Visitor<Gen<'T>> with
                member __.Visit<'t1,'t2,'t3>() =
                    let t1G,t2G,t3G = mkGenerator<'t1>(), mkGenerator<'t2>(), mkGenerator<'t3>()
                    wrap(fun r -> t1G r, t2G r, t3G r)
        }

    | Shape.Array s ->
        s.Accept {
            new IArrayVisitor<Gen<'T>> with
                member __.Visit<'t> () =
                    let tG = mkGenerator<'t>()
                    wrap(fun r -> 
                        let length = r.Next(100)
                        Array.init length (fun _ -> tG r))
        }

    | Shape.FSharpList s ->
        s.Accept {
            new IFSharpListVisitor<Gen<'T>> with
                member __.Visit<'t> () =
                    let tG = mkGenerator<'t>()
                    wrap(fun r -> 
                        let length = r.Next(100)
                        List.init length (fun _ -> tG r))
        }

    | Shape.FSharpSet s ->
        s.Accept {
            new IFSharpSetVisitor<Gen<'T>> with
                member __.Visit<'t when 't : comparison> () =
                    let tG = mkGenerator<'t []>()
                    wrap(fun r -> tG r |> Set.ofArray)
        }

    | Shape.FSharpMap s ->
        s.Accept {
            new IFSharpMapVisitor<Gen<'T>> with
                member __.Visit<'k, 'v when 'k : comparison> () =
                    let kvG = mkGenerator<('k * 'v) []>()
                    wrap(fun r -> kvG r |> Map.ofArray)
        }

    | Shape.FSharpRecord s as ts ->
        let mkRandField (p : PropertyInfo) =
            TypeShape.Create(p.PropertyType).Accept 
                { new ITypeShapeVisitor<Gen<obj>> with
                    member __.Visit<'T>() =
                        let gt = mkGenerator<'T>()
                        fun r -> gt r :> obj }

        ts.Accept { new ITypeShapeVisitor<Gen<'T>> with
            member __.Visit<'t>() =
                let fieldCtors = s.Properties |> Seq.map mkRandField |> Seq.toArray
                wrap (fun r -> fieldCtors |> Array.map (fun f -> f r) |> s.ConstructorInfo.Invoke :?> 't) }

    | Shape.FSharpUnion s as ts ->
        let mkRandField (p : PropertyInfo) =
            TypeShape.Create(p.PropertyType).Accept 
                { new ITypeShapeVisitor<Gen<obj>> with
                    member __.Visit<'T>() =
                        let gt = mkGenerator<'T>()
                        fun r -> gt r :> obj }

        ts.Accept { new ITypeShapeVisitor<Gen<'T>> with
            member __.Visit<'t>() =
                let ctors = s.Constructors |> Seq.toArray
                let uc = s.UnionCaseInfo |> Seq.map (fun u -> u.GetFields() |> Array.map mkRandField) |> Seq.toArray
                wrap (fun r -> 
                    let tag = r.Next(uc.Length)
                    let fields = uc.[tag] |> Array.map (fun uc -> uc r)
                    ctors.[tag].Invoke(null, fields) :?> 't) }

    | _ -> failwithf "Type %O does not support random value generation." typeof<'T>


let generate<'T> () = mkGenerator<'T> () (new Random())


type Foo<'T> =
    {
        Id : Guid
        Value : 'T option
        Data : Map<string, int>
    }


generate<Foo<float> list> ()