[<AutoOpen>]
module TypeShape.Tests.StagedClone

open System
open System.Runtime.Serialization
open FSharp.Quotations
open Swensen.Unquote
open FSharp.Quotations.Evaluator

open TypeShape.Core
open TypeShape.Core.Utils
open TypeShape.Core.StagingExtensions

// Simple object clone implementation used to verify implementation correctness of staged shapes

let stageCloner<'T> (self : StagedGenerator1) (e : Expr<'T>) : Expr<'T> =
    let unwrap (e : Expr<'T>) = unbox<Expr<'a>> e
    let wrap(e : Expr<'a>) = unbox<Expr<'T>> e
    let stageCloner (self : StagedGenerator1) = Expr.lam self.Generate<'a,'a>

    let stageMemberCloner (fieldShape : IShapeMember<'DeclaringType>) 
                            (src : Expr<'DeclaringType>) 
                            (tgt : Expr<'DeclaringType>) =
        fieldShape.Accept {
            new IMemberVisitor<'DeclaringType, Expr<'DeclaringType>> with
                member __.Visit (shape : ShapeMember<'DeclaringType, 'Field>) =
                    <@
                        let sourceField = (% shape.GetExpr src)
                        let clonedField = (% stageCloner self) sourceField
                        (% Expr.lam (shape.SetExpr tgt)) clonedField
                    @>
        }

    match shapeof<'T> with
    | Shape.Primitive
    | Shape.TimeSpan
    | Shape.DateTimeOffset
    | Shape.DateTime
    | Shape.BigInt
    | Shape.Unit
    | Shape.Decimal
    | Shape.Enum _ -> <@ %e @>
    | Shape.String -> wrap <@ %e @>
    | Shape.Nullable s ->
        s.Accept { new INullableVisitor<Expr<'T>> with
            member __.Visit<'t when 't : (new : unit -> 't) 
                                and 't :> ValueType 
                                and 't : struct> () =

                // work around unquote bug
                let hasValue (x:Nullable<'t>) = x.HasValue

                wrap 
                    <@ 
                        match (% unwrap e) : Nullable<'t> with 
                        | x when hasValue x -> Nullable((% stageCloner self) x.Value)
                        | x -> x 
                    @> }

    | Shape.Array s when s.Rank = 1 ->
        s.Element.Accept { new ITypeVisitor<Expr<'T>> with
            member __.Visit<'t> () =
                if typeof<'t>.IsPrimitive then
                    wrap <@ match (% unwrap e) : 't[] with null -> null | ts -> ts.Clone() :?> 't[] @>
                else
                    wrap <@ Array.map (% stageCloner self) (% unwrap e) :'t[] @> }

    | Shape.FSharpList s ->
        s.Element.Accept { new ITypeVisitor<Expr<'T>> with
            member __.Visit<'t> () =
                wrap <@ List.map (% stageCloner self) (% unwrap e) : 't list @> }

    | Shape.FSharpSet s ->
        s.Accept {
            new IFSharpSetVisitor<Expr<'T>> with
                member __.Visit<'t when 't : comparison> () =
                    wrap <@ Set.map (% stageCloner self) (% unwrap e) : Set<'t> @> }

    | Shape.FSharpMap s ->
        s.Accept {
            new IFSharpMapVisitor<Expr<'T>> with
                member __.Visit<'k, 'v when 'k : comparison> () =
                    <@ 
                        ((% unwrap e) : Map<'k, 'v>)
                        |> Map.toSeq
                        |> Seq.map (fun (k,v) -> (% stageCloner self) k, (% stageCloner self) v)
                        |> Map.ofSeq 
                    @> |> wrap }

    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
        shape.Elements 
        |> Array.map (fun sf -> stageMemberCloner sf e)
        |> Expr.update ("target", shape.CreateUninitializedExpr())

    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
        shape.Fields
        |> Array.map (fun sf -> stageMemberCloner sf e)
        |> Expr.update ("target", shape.CreateUninitializedExpr())

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
        let mkUnionCaseCloner (case : ShapeFSharpUnionCase<'T>) =
            case.Fields
            |> Array.map (fun sf -> stageMemberCloner sf e)
            |> Expr.update ("target", case.CreateUninitializedExpr())

        let tag = shape.GetTagExpr e
        let unionCaseCloners = shape.UnionCases |> Array.map mkUnionCaseCloner
        Expr.switch tag unionCaseCloners

    | Shape.ISerializable s ->
        s.Accept { new ISerializableVisitor<Expr<'T>> with
            member __.Visit (shape : ShapeISerializable<'S>) =
                <@
                    let sc = new StreamingContext()
                    let si = new SerializationInfo(typeof<'S>, FormatterConverter())
                    ((% unwrap e) : 'S).GetObjectData(si, sc)
                    (% Expr.lam2 shape.CreateExpr) si sc
                @> |> wrap
        }

    | Shape.CliMutable (:? ShapeCliMutable<'T> as shape) ->
        shape.Properties
        |> Array.map (fun sp -> stageMemberCloner sp e)
        |> Expr.update ("target", shape.CreateUninitializedExpr())

    | Shape.Poco (:? ShapePoco<'T> as shape) ->
        shape.Fields
        |> Array.map (fun sf -> stageMemberCloner sf e)
        |> Expr.update ("target", shape.CreateUninitializedExpr())

    | _ -> failwithf "Unsupported type %O" typeof<'T>

let private cache = new TypeCache()
let mkCloneExpr<'T> () : Expr<'T -> 'T> =
    let mkCloneExprInner() =
        let recGen = 
            fun self ->
                { new StagedGenerator1 with 
                    member __.Generate<'a,'b> (e:Expr<'a>) : Expr<'b> = 
                        stageCloner<'a> self e |> unbox }
            |> Expr.Y1

        recGen.Generate<'T, 'T>
        |> Expr.lam
        |> Expr.cleanup

    match cache.TryFind<Expr<'T -> 'T>> () with
    | Some e -> e
    | None ->
        let e = mkCloneExprInner()
        let _ = cache.ForceAdd e
        e

let mkStagedCloner<'T> () = mkCloneExpr<'T>() |> eval
let mkCompiledCloner<'T> () = mkCloneExpr<'T>() |> QuotationEvaluator.Evaluate
let decompileCloner<'T> () = mkCloneExpr<'T>() |> decompile
