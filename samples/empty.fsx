#r "../bin/TypeShape.dll"

// structural empty value generator

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Reflection
open TypeShape

let rec private cache = new ConcurrentDictionary<Type, obj>()
and private mkEmptyFunc<'T> () : unit -> 'T = cache.GetOrAdd(typeof<'T>, fun _ -> aux<'T> () :> obj) :?> _
and private aux<'T> () : unit -> 'T =  
    let wrap (f : unit -> 'a) = unbox<unit -> 'T> f

    let mkMemberInitializer (shape : IShapeWriteMember<'DeclaringType>) =
        shape.Accept { new IWriteMemberVisitor<'DeclaringType, 'DeclaringType -> 'DeclaringType> with
            member __.Visit (shape : ShapeWriteMember<'DeclaringType, 'Field>) =
                let fe = mkEmptyFunc<'Field>()
                fun inst -> shape.Inject inst (fe ())
        }

    match TypeShape.Create<'T>() with
    | Shape.Primitive -> fun () -> Unchecked.defaultof<'T>
    | Shape.Decimal -> wrap(fun () -> 0M)
    | Shape.BigInt -> wrap(fun () -> 0I)
    | Shape.Guid -> wrap(fun () -> Guid.Empty)
    | Shape.String -> wrap(fun () -> "")
    | Shape.TimeSpan -> wrap(fun () -> TimeSpan.Zero)
    | Shape.DateTime -> wrap(fun () -> DateTime.MinValue)
    | Shape.DateTimeOffset -> wrap(fun () -> DateTimeOffset.MinValue)
    | Shape.Unit -> wrap id
    | Shape.Enum s ->
        s.Accept { new IEnumVisitor<unit -> 'T> with
            member __.Visit<'t, 'u when 't : enum<'u>>() = // 'T = 't
                let ue = mkEmptyFunc<'u>()
                wrap(fun () -> LanguagePrimitives.EnumOfValue<'u,'t>(ue()))
        }

    | Shape.Nullable s ->
        s.Accept { new INullableVisitor<unit -> 'T> with
            member __.Visit<'t when 't : struct and 't :> ValueType and 't : (new : unit -> 't)>() = // 'T = 't
                wrap(fun () -> new Nullable<'t>())
        }

    | Shape.DefaultConstructor s ->
        s.Accept { new IDefaultConstructorVisitor<unit -> 'T> with
            member __.Visit<'t when 't : (new : unit -> 't)>() = // 'T = 't
                wrap(fun () -> new 't ())
        }

    | Shape.FSharpOption s ->
        s.Accept { new IFSharpOptionVisitor<unit -> 'T> with
            member __.Visit<'t>() = // 'T = 't option
                let et = mkEmptyFunc<'t>()
                wrap(fun () -> Option<'t>.None) }

    | Shape.KeyValuePair s ->
        s.Accept { new IKeyValuePairVisitor<unit -> 'T> with
            member __.Visit<'k,'v>() = // 'T = KeyValuePair<'k,'v>
                let ke,ve = mkEmptyFunc<'k>(), mkEmptyFunc<'v>()
                wrap(fun () -> new KeyValuePair<'k,'v>(ke(),ve())) }

    | Shape.FSharpList s ->
        s.Accept { new IFSharpListVisitor<unit -> 'T> with
            member __.Visit<'t>() = wrap(fun () -> List.empty<'t>) } // 'T = 't list

    | Shape.Array s ->
        s.Accept { new IArrayVisitor<unit -> 'T> with
            member __.Visit<'t> rank = 
                match rank with
                | 1 -> wrap(fun () -> Array.empty<'t>) // 'T = 't []
                | 2 -> wrap(fun () -> Array2D.zeroCreate<'t> 0 0) // 'T = 't [,]
                | 3 -> wrap(fun () -> Array3D.zeroCreate<'t> 0 0 0) // 'T = 't [,,]
                | 4 -> wrap(fun () -> Array4D.zeroCreate<'t> 0 0 0 0) // 'T = 't [,,,]
                | _ -> failwithf "Unsupported type %O" typeof<'T> }

    | Shape.FSharpSet s ->
        s.Accept { new IFSharpSetVisitor<unit -> 'T> with
            member __.Visit<'t when 't : comparison>() = wrap(fun () -> Set.empty<'t>) } // 'T = Set<'t>

    | Shape.FSharpMap s ->
        s.Accept { new IFSharpMapVisitor<unit -> 'T> with
            member __.Visit<'k, 'v when 'k : comparison>() = wrap(fun () -> Map.empty<'k,'v>) } // 'T = Map<'k,'v>

    | Shape.Tuple s ->
        s.Accept { new ITupleVisitor<unit -> 'T> with
            member __.Visit (shape : ShapeTuple<'Tuple>) = // 'T = 'Tuple
                let elemInitializers = shape.Elements |> Array.map mkMemberInitializer
                wrap(fun () ->
                    let mutable inst = shape.CreateUninitialized()
                    for f in elemInitializers do inst <- f inst
                    inst) }


    | Shape.FSharpRecord s ->
        s.Accept { new IFSharpRecordVisitor<unit -> 'T> with
            member __.Visit (shape : ShapeFSharpRecord<'Record>) = // 'T = 'Record
                let fieldInitializers = shape.Fields |> Array.map mkMemberInitializer
                wrap(fun () ->
                    let mutable inst = shape.CreateUninitialized()
                    for f in fieldInitializers do inst <- f inst
                    inst) }

    | Shape.CliMutable s ->
        s.Accept { new ICliMutableVisitor<unit -> 'T> with
            member __.Visit (shape : ShapeCliMutable<'Class>) = // 'T = 'Class
                wrap(fun () -> shape.CreateUninitialized())
        }

    | _ -> failwithf "Type '%O' does not support empty values." typeof<'T>

and empty<'T> = mkEmptyFunc<'T> () ()

/// examples

type Record<'T> =
    {
        Id : string
        Value : 'T ref
        UUID : Guid
        MoneyMoney : decimal
        Values : (int * string) list
        Metrics : Map<string, float>
        Set : Set<string>
    }

{ empty<Record<int * string option>> with Id = "myId" ; MoneyMoney = 3.14M }