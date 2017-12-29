[<AutoOpen>]
module TypeShape.Tests.Clone

open System
open System.Runtime.Serialization
open TypeShape.Core.Utils
open TypeShape.Core

// Simple object clone implementation used to verify implementation correctness of shapes

let rec mkCloner<'T> () : 'T -> 'T =
    match cache.TryFind<'T -> 'T> () with
    | Some c -> c
    | None ->
        use ctx = cache.CreateRecTypeManager()
        mkClonerCached<'T> ctx

and private mkClonerCached<'T> (ctx : RecTypeManager) : 'T -> 'T =
    match ctx.TryFind<'T -> 'T> () with
    | Some c -> c
    | None ->
        let _ = ctx.CreateUninitialized<'T -> 'T>(fun c t -> c.Value t)
        let c = mkClonerAux<'T> ctx
        ctx.Complete c

and private mkClonerAux<'T> (ctx : RecTypeManager) : 'T -> 'T =
    let wrap(f : 'a -> 'a) = unbox<'T -> 'T> f
    let mkMemberCloner (fieldShape : IShapeWriteMember<'DeclaringType>) =
        fieldShape.Accept {
            new IWriteMemberVisitor<'DeclaringType, 'DeclaringType -> 'DeclaringType -> 'DeclaringType> with
                member __.Visit (shape : ShapeWriteMember<'DeclaringType, 'Field>) =
                    let fieldCloner = mkClonerCached<'FieldType> ctx
                    fun src tgt ->
                        let field = shape.Project src
                        let field' = fieldCloner field
                        shape.Inject tgt field'
        }

    match shapeof<'T> with
    | Shape.Primitive
    | Shape.TimeSpan
    | Shape.DateTimeOffset
    | Shape.DateTime
    | Shape.BigInt
    | Shape.Unit
    | Shape.Decimal
    | Shape.Enum _ -> id
    | Shape.String -> wrap(function null -> null | x -> String.Copy(x))
    | Shape.Array s when s.Rank = 1 ->
        s.Accept {
            new IArrayVisitor<'T -> 'T> with
                member __.Visit<'t> _ =
                    if typeof<'t>.IsPrimitive then
                        wrap(fun (ts:'t[]) -> ts.Clone() :?> 't[])
                    else
                        let ec = mkClonerCached<'t> ctx
                        wrap(Array.map ec) }

    | Shape.FSharpList s ->
        s.Accept {
            new IFSharpListVisitor<'T -> 'T> with
                member __.Visit<'t> () =
                    let ec = mkClonerCached<'t> ctx
                    wrap(List.map ec) }

    | Shape.FSharpSet s ->
        s.Accept {
            new IFSharpSetVisitor<'T -> 'T> with
                member __.Visit<'t when 't : comparison> () =
                    let tc = mkClonerCached<'t> ctx
                    wrap(Set.map tc) }

    | Shape.FSharpMap s ->
        s.Accept {
            new IFSharpMapVisitor<'T -> 'T> with
                member __.Visit<'k, 'v when 'k : comparison> () =
                    let kc = mkClonerCached<'k> ctx
                    let vc = mkClonerCached<'v> ctx
                    wrap(Map.toSeq >> Seq.map (fun (k,v) -> kc k, vc v) >> Map.ofSeq) }

    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
        let memberCloners = shape.Elements |> Array.map mkMemberCloner
        fun source ->
            let mutable target = shape.CreateUninitialized()
            for mc in memberCloners do
                target <- mc source target
            
            target

    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
        let memberCloners = shape.Fields |> Array.map mkMemberCloner
        fun source ->
            let mutable target = shape.CreateUninitialized()
            for mc in memberCloners do
                target <- mc source target
            
            target

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
        let caseMemberCloners = 
            shape.UnionCases 
            |> Array.map (fun c -> c.Fields |> Array.map mkMemberCloner)

        fun source ->
            let tag = shape.GetTag source
            let case = shape.UnionCases.[tag]
            let memberCloners = caseMemberCloners.[tag]
            let mutable target = case.CreateUninitialized()
            for mc in memberCloners do
                target <- mc source target

            target

    | Shape.ISerializable s ->
        s.Accept { new ISerializableVisitor<'T -> 'T> with
            member __.Visit (shape : ShapeISerializable<'S>) =
                fun (source : 'S) ->
                    let sc = new StreamingContext()
                    let si = new SerializationInfo(typeof<'S>, FormatterConverter())
                    source.GetObjectData(si, sc)
                    shape.Create(si, sc)
                |> wrap }

    | Shape.CliMutable (:? ShapeCliMutable<'T> as shape) ->
        let memberCloners = shape.Properties |> Array.map mkMemberCloner
        fun source ->
            let mutable target = shape.CreateUninitialized()
            for mc in memberCloners do
                target <- mc source target
            
            target

    | Shape.Poco (:? ShapePoco<'T> as shape) ->
        let fieldCloners = shape.Fields |> Array.map mkMemberCloner
        fun source ->
            let mutable target = shape.CreateUninitialized()
            for fc in fieldCloners do
                target <- fc source target

            target
                

    | _ -> failwithf "Unsupported type %O" typeof<'T>

and private cache : TypeCache = new TypeCache()