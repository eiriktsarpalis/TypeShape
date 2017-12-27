[<AutoOpen>]
module TypeShape.Tests.StagedClone

open System
open System.Runtime.Serialization
open FSharp.Quotations
open Swensen.Unquote
open TypeShape.Core
open TypeShape.Core.StagingExtensions

// Simple object clone implementation used to verify implementation correctness of staged shapes

type CloneExpr<'T> = Expr<'T> -> Expr<'T>

let rec stageCloner<'T> () : CloneExpr<'T> =
    let wrap(f : CloneExpr<'a>) = unbox<CloneExpr<'T>> f

    let stageMemberCloner (fieldShape : IShapeWriteMember<'DeclaringType>) =
        fieldShape.Accept {
            new IWriteMemberVisitor<'DeclaringType, Expr<'DeclaringType> -> Expr<'DeclaringType> -> Expr<'DeclaringType>> with
                member __.Visit (shape : ShapeWriteMember<'DeclaringType, 'Field>) =
                    let fieldCloner = stageCloner<'FieldType>()
                    fun src tgt ->
                        <@
                            let sourceField = (% shape.ProjectExpr src)
                            let clonedField = (% Expr.lam fieldCloner) sourceField
                            (% Expr.lam (shape.InjectExpr tgt)) clonedField
                        @>
        }

    match shapeof<'T> with
    | Shape.Primitive
    | Shape.TimeSpan
    | Shape.DateTimeOffset
    | Shape.DateTime
    | Shape.BigInt
    | Shape.Unit
    | Shape.Decimal -> fun t -> <@ %t @>
    | Shape.String -> wrap(fun (s:Expr<string>) -> <@ match %s with null -> null | s -> String.Copy s @>)
    | Shape.Array s when s.Rank = 1 ->
        s.Accept { new IArrayVisitor<CloneExpr<'T>> with
            member __.Visit<'t> _ =
                wrap(fun (ts:Expr<'t[]>) ->
                    if typeof<'t>.IsPrimitive then
                        <@ match %ts with null -> null | ts -> ts.Clone() :?> 't[] @>
                    else
                        let ec = stageCloner<'t>() 
                        <@ Array.map (% Expr.lam ec) %ts @>) }

    | Shape.FSharpList s ->
        s.Accept { new IFSharpListVisitor<CloneExpr<'T>> with
            member __.Visit<'t> () =
                wrap(fun (ts:Expr<'t list>) ->
                    let ec = stageCloner<'t>()
                    <@ List.map (% Expr.lam ec) %ts @> ) }

    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
        fun source ->
            shape.Elements 
            |> Array.map (fun sf -> stageMemberCloner sf source)
            |> Expr.update ("target", shape.CreateUninitializedExpr())

    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
        fun source ->
            shape.Fields
            |> Array.map (fun sf -> stageMemberCloner sf source)
            |> Expr.update ("target", shape.CreateUninitializedExpr())

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
        fun source ->
            let mkUnionCaseCloner (case : ShapeFSharpUnionCase<'T>) =
                case.Fields
                |> Array.map (fun sf -> stageMemberCloner sf source)
                |> Expr.update ("target", case.CreateUninitializedExpr())

            let tag = shape.GetTagExpr source
            let unionCaseCloners = shape.UnionCases |> Array.map mkUnionCaseCloner
            Expr.switch tag unionCaseCloners

    | Shape.ISerializable s ->
        s.Accept { new ISerializableVisitor<CloneExpr<'T>> with
            member __.Visit (shape : ShapeISerializable<'S>) =
                fun (source : Expr<'S>) ->
                    <@
                        let sc = new StreamingContext()
                        let si = new SerializationInfo(typeof<'S>, FormatterConverter())
                        (%source).GetObjectData(si, sc)
                        (% Expr.lam2 shape.CreateExpr) si sc
                    @>
                |> wrap
        }

    | Shape.CliMutable (:? ShapeCliMutable<'T> as shape) ->
        fun source ->
            shape.Properties
            |> Array.map (fun sp -> stageMemberCloner sp source)
            |> Expr.update ("target", shape.CreateUninitializedExpr())

    | Shape.Poco (:? ShapePoco<'T> as shape) ->
        fun source ->
            shape.Fields
            |> Array.map (fun sf -> stageMemberCloner sf source)
            |> Expr.update ("target", shape.CreateUninitializedExpr())

    | _ -> failwithf "Unsupported type %O" typeof<'T>

let mkCloneExpr<'T> () = stageCloner<'T>() |> Expr.lam |> Expr.cleanup
let mkStagedCloner<'T> () = mkCloneExpr<'T>() |> eval
let decompileCloner<'T> () = mkCloneExpr<'T>() |> decompile