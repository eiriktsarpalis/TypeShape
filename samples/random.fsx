#r "../bin/TypeShape.dll"
#r "../bin/FsCheck.dll"

open System
open System.Reflection
open TypeShape
open FsCheck

// Generic random value generator for FsCheck
// Supports C# records and POCOs

let rec mkGenerator<'T> () : Gen<'T> =
    let wrap (t : Gen<'a>) = unbox<Gen<'T>> t
    let mkRandomMember (shape : IShapeWriteMember<'DeclaringType>) =
        shape.Accept { new IWriteMemberVisitor<'DeclaringType, Gen<'DeclaringType -> 'DeclaringType>> with
            member __.Visit (shape : ShapeWriteMember<'DeclaringType, 'Field>) =
                let rf = mkGenerator<'Field>()
                gen { let! f = rf in return fun dt -> shape.Inject dt f } }

    match TypeShape.Create<'T>() with
    | s when s.Type.IsPrimitive -> wrap Arb.generate<'T>
    | Shape.Unit -> wrap Arb.generate<unit>
    | Shape.String -> wrap Arb.generate<string>
    | Shape.Guid -> wrap Arb.generate<Guid>
    | Shape.DateTime -> wrap Arb.generate<DateTime>
    | Shape.FSharpOption s ->
        s.Accept { new IFSharpOptionVisitor<Gen<'T>> with
            member __.Visit<'t> () =
                let tGen = mkGenerator<'t>()
                Gen.frequency 
                    [ (10, tGen |> Gen.map Some) ; 
                        (1, gen { return None }) ]
                |> wrap
        }

    | Shape.Array s when s.Rank = 1 ->
        s.Accept { new IArrayVisitor<Gen<'T>> with
            member __.Visit<'t> _ =
                let tG = mkGenerator<'t>()
                gen {
                    let! length = Gen.sized(fun n -> Gen.choose(-1, n))
                    match length with
                    | -1 -> return null
                    | _ ->
                        let array = Array.zeroCreate<'t> length
                        for i = 0 to array.Length - 1 do let! t = tG in array.[i] <- t
                        return array
                } |> wrap
        }

    | Shape.FSharpList s ->
        s.Accept { new IFSharpListVisitor<Gen<'T>> with
            member __.Visit<'t> () =
                let tG = mkGenerator<'t>()
                gen {
                    let! length = Gen.sized(fun n -> Gen.choose(0, n))
                    let rec aux acc n = gen {
                        if n = 0 then return acc
                        else
                            let! t = tG
                            return! aux (t :: acc) (n - 1)
                    }

                    return! aux [] length
                } |> wrap
        }

    | Shape.FSharpSet s ->
        s.Accept { new IFSharpSetVisitor<Gen<'T>> with
            member __.Visit<'t when 't : comparison> () =
                let tG = mkGenerator<'t list>()
                wrap(tG |> Gen.map Set.ofList)
        }

    | Shape.FSharpMap s ->
        s.Accept {
            new IFSharpMapVisitor<Gen<'T>> with
                member __.Visit<'k, 'v when 'k : comparison> () =
                    let kvG = mkGenerator<('k * 'v) list>()
                    wrap(kvG |> Gen.map Map.ofList)
        }

    | Shape.Tuple s ->
        s.Accept { new ITupleVisitor<Gen<'T>> with
            member __.Visit (shape : ShapeTuple<'Tuple>) =
                let eGens = shape.Elements |> Array.map mkRandomMember
                gen {
                    let mutable target = shape.CreateUninitialized()
                    for eg in eGens do let! u = eg in target <- u target
                    return target
                } |> wrap
        }

    | Shape.FSharpRecord s ->
        s.Accept { new IFSharpRecordVisitor<Gen<'T>> with
            member __.Visit (shape : ShapeFSharpRecord<'Record>) =
                let fieldGen = shape.Fields |> Array.map mkRandomMember
                gen {
                    let mutable target = shape.CreateUninitialized()
                    for eg in fieldGen do let! u = eg in target <- u target
                    return target
                } |> wrap
        }

    | Shape.FSharpUnion s ->
        s.Accept { new IFSharpUnionVisitor<Gen<'T>> with
            member __.Visit (shape : ShapeFSharpUnion<'Union>) =
                let caseFieldGen = shape.UnionCases |> Array.map (fun uc -> uc.Fields |> Array.map mkRandomMember)
                gen {
                    let! tag = Gen.choose(0, caseFieldGen.Length - 1)
                    let mutable u = shape.UnionCases.[tag].CreateUninitialized()
                    for f in caseFieldGen.[tag] do let! uf = f in u <- uf u
                    return u
                } |> wrap
        }

    | Shape.CliMutable s ->
        s.Accept { new ICliMutableVisitor<Gen<'T>> with
            member __.Visit (shape : ShapeCliMutable<'Class>) =
                let propGen = shape.Properties |> Array.map mkRandomMember
                gen {
                    let mutable target = shape.CreateUninitialized()
                    for ep in propGen do let! up = ep in target <- up target
                    return target
                } |> wrap
        }

    | Shape.Poco s ->
        s.Accept { new IPocoVisitor<Gen<'T>> with
            member __.Visit (shape : ShapePoco<'Poco>) =
                let bestCtor =
                    shape.Constructors 
                    |> Seq.filter (fun c -> c.IsPublic) 
                    |> Seq.sortBy (fun c -> c.Arity) 
                    |> Seq.tryFind (fun _ -> true)

                match bestCtor with
                | None -> failwithf "Class %O lacking an appropriate ctor" typeof<'Poco>
                | Some ctor ->

                ctor.Accept { new IConstructorVisitor<'CtorParams, Gen<'T>> with
                    member __.Visit<'CtorParams> (ctor : ShapeConstructor<'Poco, 'CtorParams>) =
                        let paramGen = mkGenerator<'CtorParams>()
                        gen {
                            let! args = paramGen
                            return ctor.Invoke args
                        } |> wrap
                }
        }

    | _ -> failwithf "Type %O does not support random value generation." typeof<'T>


//--------------------------------------
// Example

type Customer() =
    member val Name = "" with get, set
    member val Age = 0 with get, set
    member val DateJoined = DateTime.MinValue with get, set

type Poco(name : string, age : int) =
    member __.Name = name
    member __.Age = age

type Foo<'T> =
    {
        Id : Guid
        Poco : Poco
        Value : 'T option
        Data : Map<string, int>
    }

//let gen = Arb.generate<Foo<Customer> list> // not supported
let gen = mkGenerator<Foo<Customer> list> ()

Gen.sample 10 10 gen