[<AutoOpen>]
module TypeShape.Tests.StagedClone

open System
open FSharp.Quotations
open TypeShape
open TypeShape_StagingExtensions
open Swensen.Unquote

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

    match TypeShape.Create<'T>() with
    | _ when typeof<'T>.IsPrimitive -> fun t -> <@ %t @>
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

    | Shape.Tuple s ->
        s.Accept { new ITupleVisitor<CloneExpr<'T>> with
            member __.Visit (shape : ShapeTuple<'Tuple>) =
                wrap(fun (source : Expr<'Tuple>) ->
                    shape.Elements 
                    |> Array.map (fun sf -> stageMemberCloner sf source)
                    |> Expr.update ("target", shape.CreateUninitializedExpr())) }

    | Shape.FSharpRecord s ->
        s.Accept { new IFSharpRecordVisitor<CloneExpr<'T>> with
            member __.Visit (shape : ShapeFSharpRecord<'R>) =
                wrap(fun (source : Expr<'R>) ->
                    shape.Fields
                    |> Array.map (fun sf -> stageMemberCloner sf source)
                    |> Expr.update ("target", shape.CreateUninitializedExpr())) }

    | Shape.FSharpUnion s ->
        s.Accept { new IFSharpUnionVisitor<CloneExpr<'T>> with
            member __.Visit (shape : ShapeFSharpUnion<'U>) =
                wrap(fun (source : Expr<'U>) ->
                    let mkUnionCaseCloner (case : ShapeFSharpUnionCase<'U>) =
                        case.Fields
                        |> Array.map (fun sf -> stageMemberCloner sf source)
                        |> Expr.update ("target", case.CreateUninitializedExpr())

                    let tag = shape.GetTagExpr source
                    let unionCaseCloners = shape.UnionCases |> Array.map mkUnionCaseCloner
                    Expr.switch tag unionCaseCloners) }

    | Shape.CliMutable s ->
        s.Accept { new ICliMutableVisitor<CloneExpr<'T>> with
            member __.Visit<'R> (shape : ShapeCliMutable<'R>) =
                wrap(fun (source : Expr<'R>) ->
                    shape.Properties
                    |> Array.map (fun sp -> stageMemberCloner sp source)
                    |> Expr.update ("target", shape.CreateUninitializedExpr())) }

    | Shape.Poco s ->
        s.Accept { new IPocoVisitor<CloneExpr<'T>> with
            member __.Visit (shape : ShapePoco<'P>) =
                wrap(fun (source : Expr<'P>) ->
                    shape.Fields
                    |> Array.map (fun sf -> stageMemberCloner sf source)
                    |> Expr.update ("target", shape.CreateUninitializedExpr())) }

    | _ -> failwithf "Unsupported type %O" typeof<'T>

let mkCloneExpr<'T> () = stageCloner<'T>() |> Expr.lam |> Expr.cleanup
let mkStagedCloner<'T> () = mkCloneExpr<'T>() |> eval
let decompileCloner<'T> () = mkCloneExpr<'T>() |> decompile