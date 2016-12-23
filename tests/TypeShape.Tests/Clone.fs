[<AutoOpen>]
module TypeShape.Tests.Clone

open System
open TypeShape

// Simple object clone implementation used to verify implementation correctness of shapes

let rec mkCloner<'T> () : 'T -> 'T =
    let wrap(f : 'a -> 'a) = unbox<'T -> 'T> f
    let mkMemberCloner (fieldShape : IShapeWriteMember<'DeclaringType>) =
        fieldShape.Accept {
            new IWriteMemberVisitor<'DeclaringType, 'DeclaringType -> 'DeclaringType -> 'DeclaringType> with
                member __.Visit (shape : ShapeWriteMember<'DeclaringType, 'Field>) =
                    let fieldCloner = mkCloner<'FieldType>()
                    fun src tgt ->
                        let field = shape.Project src
                        let field' = fieldCloner field
                        shape.Inject tgt field'
        }

    match TypeShape.Create<'T>() with
    | _ when typeof<'T>.IsPrimitive -> id
    | Shape.TimeSpan
    | Shape.DateTimeOffset
    | Shape.DateTime
    | Shape.BigInt
    | Shape.Unit
    | Shape.Decimal -> id
    | Shape.String -> wrap(function null -> null | x -> String.Copy(x))
    | Shape.Array s when s.Rank = 1 ->
        s.Accept {
            new IArrayVisitor<'T -> 'T> with
                member __.Visit<'t> _ =
                    if typeof<'t>.IsPrimitive then
                        wrap(fun (ts:'t[]) -> ts.Clone() :?> 't[])
                    else
                        let ec = mkCloner<'t>()
                        wrap(Array.map ec) }

    | Shape.FSharpList s ->
        s.Accept {
            new IFSharpListVisitor<'T -> 'T> with
                member __.Visit<'t> () =
                    let ec = mkCloner<'t>()
                    wrap(List.map ec) }

    | Shape.Tuple s ->
        s.Accept {
            new ITupleVisitor<'T -> 'T> with
                member __.Visit (shape : ShapeTuple<'Tuple>) =
                    let memberCloners = shape.Elements |> Array.map mkMemberCloner
                    wrap(fun (source : 'Tuple) ->
                        let mutable target = shape.CreateUninitialized()
                        for mc in memberCloners do
                            target <- mc source target
                        
                        target) }

    | Shape.FSharpRecord s ->
        s.Accept {
            new IFSharpRecordVisitor<'T -> 'T> with
                member __.Visit (shape : ShapeFSharpRecord<'R>) =
                    let memberCloners = shape.Fields |> Array.map mkMemberCloner
                    wrap(fun (source:'R) ->
                        let mutable target = shape.CreateUninitialized()
                        for mc in memberCloners do
                            target <- mc source target
                        
                        target) }

    | Shape.FSharpUnion s ->
        s.Accept {
            new IFSharpUnionVisitor<'T -> 'T> with
                member __.Visit (shape : ShapeFSharpUnion<'U>) =
                    let caseMemberCloners = 
                        shape.UnionCases 
                        |> Array.map (fun c -> c.Fields |> Array.map mkMemberCloner)

                    wrap(fun (source:'U) ->
                        let tag = shape.GetTag source
                        let case = shape.UnionCases.[tag]
                        let memberCloners = caseMemberCloners.[tag]
                        let mutable target = case.CreateUninitialized()
                        for mc in memberCloners do
                            target <- mc source target

                        target) }

    | Shape.CliMutable s ->
        s.Accept {
            new ICliMutableVisitor<'T -> 'T> with
                member __.Visit<'R> (shape : ShapeCliMutable<'R>) =
                    let memberCloners = shape.Properties |> Array.map mkMemberCloner
                    wrap(fun (source:'R) ->
                        let mutable target = shape.CreateUninitialized()
                        for mc in memberCloners do
                            target <- mc source target
                        
                        target) }

    | Shape.Poco s ->
        s.Accept {
            new IPocoVisitor<'T -> 'T> with
                member __.Visit (shape : ShapePoco<'P>) =
                    let fieldCloners = shape.Fields |> Array.map mkMemberCloner
                    wrap(fun (source : 'P) ->
                        let mutable target = shape.CreateUninitialized()
                        for fc in fieldCloners do
                            target <- fc source target

                        target) }

    | _ -> failwithf "Unsupported type %O" typeof<'T>