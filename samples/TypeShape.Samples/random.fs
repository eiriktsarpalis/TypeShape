module Random

open System
open TypeShape.Core
open FsCheck
open FsCheck.FSharp

// Generic random value generator for FsCheck
// Supports C# records and POCOs

let defaultGen<'a> : Gen<'a> = ArbMap.generate<'a> ArbMap.defaults

let rec mkGenerator<'T> () : Gen<'T> =
    let wrap (t : Gen<'a>) = unbox<Gen<'T>> t
    let mkRandomMember (shape : IShapeMember<'DeclaringType>) =
        shape.Accept { new IMemberVisitor<'DeclaringType, Gen<'DeclaringType -> 'DeclaringType>> with
            member _.Visit (shape : ShapeMember<'DeclaringType, 'Field>) =
                let rf = mkGenerator<'Field>()
                gen { let! f = rf in return fun dt -> shape.Set dt f } }

    match shapeof<'T> with
    | Shape.Primitive -> wrap defaultGen<'T>
    | Shape.Unit -> wrap defaultGen<unit>
    | Shape.String -> wrap defaultGen<string>
    | Shape.Guid -> wrap defaultGen<Guid>
    | Shape.DateTime -> wrap defaultGen<DateTime>
    | Shape.FSharpOption s ->
        s.Element.Accept { new ITypeVisitor<Gen<'T>> with
            member _.Visit<'t> () =
                let tGen = mkGenerator<'t>()
                Gen.frequency 
                    [ (10, tGen |> Gen.map Some) ; 
                        (1, gen { return None }) ]
                |> wrap
        }

    | Shape.Array s when s.Rank = 1 ->
        s.Element.Accept { new ITypeVisitor<Gen<'T>> with
            member _.Visit<'t> () =
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
        s.Element.Accept { new ITypeVisitor<Gen<'T>> with
            member _.Visit<'t> () =
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
            member _.Visit<'t when 't : comparison> () =
                let tG = mkGenerator<'t list>()
                wrap(tG |> Gen.map Set.ofList)
        }

    | Shape.FSharpMap s ->
        s.Accept {
            new IFSharpMapVisitor<Gen<'T>> with
                member _.Visit<'k, 'v when 'k : comparison> () =
                    let kvG = mkGenerator<('k * 'v) list>()
                    wrap(kvG |> Gen.map Map.ofList)
        }

    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
        let eGens = shape.Elements |> Array.map mkRandomMember
        gen {
            let mutable target = shape.CreateUninitialized()
            for eg in eGens do let! u = eg in target <- u target
            return target
        }

    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
        let fieldGen = shape.Fields |> Array.map mkRandomMember
        gen {
            let mutable target = shape.CreateUninitialized()
            for eg in fieldGen do let! u = eg in target <- u target
            return target
        }

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
        let caseFieldGen = shape.UnionCases |> Array.map (fun uc -> uc.Fields |> Array.map mkRandomMember)
        gen {
            let! tag = Gen.choose(0, caseFieldGen.Length - 1)
            let mutable u = shape.UnionCases.[tag].CreateUninitialized()
            for f in caseFieldGen.[tag] do let! uf = f in u <- uf u
            return u
        }

    | Shape.CliMutable (:? ShapeCliMutable<'T> as shape) ->
        let propGen = shape.Properties |> Array.map mkRandomMember
        gen {
            let mutable target = shape.CreateUninitialized()
            for ep in propGen do let! up = ep in target <- up target
            return target
        }

    | Shape.Poco (:? ShapePoco<'T> as shape) ->
        let bestCtor =
            shape.Constructors 
            |> Seq.filter (fun c -> c.IsPublic) 
            |> Seq.sortBy (fun c -> c.Arity) 
            |> Seq.tryFind (fun _ -> true)

        match bestCtor with
        | None -> failwithf "Class %O lacking an appropriate ctor" typeof<'T>
        | Some ctor ->

        ctor.Accept { new IConstructorVisitor<'T, Gen<'T>> with
            member _.Visit<'CtorParams> (ctor : ShapeConstructor<'T, 'CtorParams>) =
                let paramGen = mkGenerator<'CtorParams>()
                gen {
                    let! args = paramGen
                    return ctor.Invoke args
                }
        }

    | _ -> defaultGen<'T> // fall back to FsCheck mechanism


//--------------------------------------
// Example

type Person(name : string, age : int) =
    member _.Name = name
    member _.Age = age
    override p.ToString() = sprintf "{ Name = \"%s\" ; Age = %d }" p.Name p.Age

type Customer() =
    member val Person = Unchecked.defaultof<Person> with get, set
    member val DateJoined = Unchecked.defaultof<DateTimeOffset> with get, set
    member val Balance = Unchecked.defaultof<decimal> with get, set

//let gen = Arb.generate<Customer list> // not supported
let gen = mkGenerator<Customer list> ()

Gen.sample 3 gen