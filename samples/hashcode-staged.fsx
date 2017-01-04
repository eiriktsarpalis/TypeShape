#r "../bin/TypeShape.dll"
#r "../bin/Unquote.dll"

open TypeShape
open TypeShape_StagingExtensions
open Swensen.Unquote
open FSharp.Quotations

type HashExpr<'T> = Expr<'T> -> Expr<int>

let rec stageHasher<'T> () : HashExpr<'T> =
    let wrap (cmp : HashExpr<'a>) = unbox<HashExpr<'T>> cmp

    let combineHash (h1 : Expr<int>) (h2 : Expr<int>) =
        <@ let h1 = %h1 in let h2 = %h2 in ((h1 <<< 5) + h1) ||| h2 @>

    let stageMemberHash (shape : IShapeMember<'DeclaringType>) =
        shape.Accept { new IMemberVisitor<'DeclaringType, HashExpr<'DeclaringType>> with
            member __.Visit (shape : ShapeMember<'DeclaringType, 'FieldType>) =
                let fhash = stageHasher<'FieldType>()
                fun dt -> fhash(shape.ProjectExpr dt) }

    match TypeShape.Create<'T> () with
    | Shape.Unit -> wrap(fun (_: Expr<unit>) -> <@ 0 @>)
    | Shape.Bool -> wrap(fun (b: Expr<bool>) -> <@ if %b then 1 else 0 @>)
    | Shape.Int32 -> wrap(fun (n: Expr<int>) -> <@ %n @>)
    | Shape.Double -> wrap(fun (d: Expr<double>) -> <@ hash %d @>)
    | Shape.String -> wrap(fun (s: Expr<string>) -> <@ hash %s @>)
    | Shape.Array s when s.Rank = 1 ->
        s.Accept { new IArrayVisitor<HashExpr<'T>> with
            member __.Visit<'t> _ =
                wrap(fun (ts : Expr<'t []>) ->
                    let eh = stageHasher<'t>()
                    <@
                        match %ts with
                        | null -> 0
                        | ts ->
                            let mutable agg = 0
                            for t in ts do
                                let th = (% Expr.lam eh) t
                                agg <- (% Expr.lam2 combineHash) agg th
                            agg
                    @> )}

    | Shape.FSharpOption s ->
        s.Accept { new IFSharpOptionVisitor<HashExpr<'T>> with
            member __.Visit<'t> () =
                wrap(fun topt ->
                    let eh = stageHasher<'t> ()
                    <@
                        match %topt with
                        | None -> 0
                        | Some t -> 
                            let th = (% Expr.lam eh) t
                            (% Expr.lam2 combineHash) 1 th
                    @> )}

    | Shape.FSharpList s ->
        s.Accept { new IFSharpListVisitor<HashExpr<'T>> with
            member __.Visit<'t> () =
                wrap(fun (ts : Expr<'t list>) ->
                    let eh = stageHasher<'t> ()
                    <@
                        let mutable agg = 0
                        for t in %ts do
                            let th = (% Expr.lam eh) t
                            agg <- (% Expr.lam2 combineHash) agg th

                        agg
                    @> ) }

    | Shape.Tuple s ->
        s.Accept { new ITupleVisitor<HashExpr<'T>> with
            member __.Visit (shape : ShapeTuple<'Tuple>) =
                wrap (fun (tuple : Expr<'Tuple>) ->
                    let mkElementHasher tuple =
                        shape.Elements
                        |> Array.map (fun e -> stageMemberHash e tuple)
                        |> Array.map (fun eh agg -> combineHash eh agg)
                        |> Expr.update ("agg", <@ 0 @>)

                    <@
                        let tuple = %tuple
                        (% Expr.lam mkElementHasher) tuple
                    @>) }

    | Shape.FSharpRecord s ->
        s.Accept { new IFSharpRecordVisitor<HashExpr<'T>> with
            member __.Visit (shape : ShapeFSharpRecord<'Record>) =
                wrap (fun (record : Expr<'Record>) ->
                    let mkFieldHasher record =
                        shape.Fields
                        |> Array.map (fun e -> stageMemberHash e record)
                        |> Array.map (fun eh agg -> combineHash eh agg)
                        |> Expr.update ("agg", <@ 0 @>)
                            
                    <@
                        let record = %record
                        (% Expr.lam mkFieldHasher) record
                    @> )}

    | Shape.FSharpUnion s ->
        s.Accept { new IFSharpUnionVisitor<HashExpr<'T>> with
            member __.Visit (shape : ShapeFSharpUnion<'Union>) =
                wrap(fun (u : Expr<'Union>) ->
                    let stageUnionCaseHasher 
                        (union : Expr<'Union>) (tag : Expr<int>)
                        (case : ShapeFSharpUnionCase<'Union>) =
                    
                        case.Fields
                        |> Array.map (fun c -> stageMemberHash c union)
                        |> Array.map (fun fh agg -> combineHash fh agg)
                        |> Expr.update ("agg", tag)

                    let stageUnionCaseHashers (u : Expr<'Union>) (tag : Expr<int>) =
                        shape.UnionCases
                        |> Array.map (stageUnionCaseHasher u tag)
                        |> Expr.switch tag

                    <@
                        let union = %u
                        let tag = (% Expr.lam shape.GetTagExpr) union
                        (% Expr.lam2 stageUnionCaseHashers) union tag  
                    @> )}

    | _ -> failwithf "Unsupported shape %O" typeof<'T>

let mkHashCodeExpr<'T>() = stageHasher<'T>() |> Expr.lam |> Expr.cleanup
let mkHasher<'T> () = mkHashCodeExpr<'T>() |> eval
let decompileHasher<'T> () = mkHashCodeExpr<'T>() |> decompile


// examples

let hasher = mkHasher<int list * string option>()

hasher ([1 .. 100], Some "42")

decompileHasher<int * (string * bool)>()
//fun t -> 
//    let mutable agg = 0 
//    agg <- let h1 = t.m_Item1 in (h1 <<< 5) + h1 ||| agg
//    agg <- 
//        let h1 = 
//            let tuple = t.m_Item2 
//            let mutable agg = 0 
//            agg <- let h1 = hash tuple.m_Item1 in (h1 <<< 5) + h1 ||| agg
//            agg <- let h1 = if tuple.m_Item2 then 1 else 0 in (h1 <<< 5) + h1 ||| agg
//            agg 
//        (h1 <<< 5) + h1 ||| agg 
//    agg
   
type Foo = { A : int ; B : string }

type Bar =
    | UA
    | UB of foo:string
    | UC of Foo

let hasher' = mkHasher<Bar>()

hasher' (UC { A = 12 ; B = "test" })

decompileHasher<Bar>()
//fun t -> 
//    let tag = t.Tag 
//    if tag = 0 then tag 
//    elif tag = 1 then 
//        let mutable agg = tag 
//        agg <- let h1 = hash t._foo in (h1 <<< 5) + h1 ||| agg
//        agg 
//    elif tag = 2 then 
//        let mutable agg = tag 
//        agg <- 
//            let h1 = 
//                let record = t.item 
//                let mutable agg = 0 
//                agg <- let h1 = record.A@ in (h1 <<< 5) + h1 ||| agg
//                agg <- let h1 = record.B@.GetHashCode() in (h1 <<< 5) + h1 ||| agg
//                agg 
//            (h1 <<< 5) + h1 ||| agg
//        agg 
//    else invalidOp "invalid tag"