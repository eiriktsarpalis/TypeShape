module HashCode_Staged

open FSharp.Quotations
open FSharp.Quotations.Evaluator
open Swensen.Unquote
open TypeShape.Core
open TypeShape.Core.StagingExtensions

// Staged hashcode generation

let mkStagedHasher<'T> (self : StagedGenerator1) (expr : Expr<'T>) : Expr<int> =
    let unwrap () = unbox<Expr<'a>> expr
    let genHash () = Expr.lam (fun e -> self.Generate<'a, int> e)

    let combineHash (h1 : Expr<int>) (h2 : Expr<int>) =
        <@ let h1 = %h1 in let h2 = %h2 in ((h1 <<< 5) + h1) ||| h2 @>

    let stageMemberHash (shape : IShapeReadOnlyMember<'DeclaringType>) (dExpr : Expr<'DeclaringType>) =
        shape.Accept { new IReadOnlyMemberVisitor<'DeclaringType, Expr<int>> with
            member _.Visit (shape : ReadOnlyMember<'DeclaringType, 'FieldType>) =
                let fExpr = shape.GetExpr dExpr
                self.Generate fExpr }

    match shapeof<'T> with
    | Shape.Unit -> <@ 0 @>
    | Shape.Bool -> <@ if (% unwrap()) then 1 else 0 @>
    | Shape.Int32 -> <@ (% unwrap()) @>
    | Shape.Double -> <@ hash<double> (% unwrap()) @>
    | Shape.String -> <@ hash<string> (% unwrap()) @>
    | Shape.Array s when s.Rank = 1 ->
        s.Element.Accept { new ITypeVisitor<Expr<int>> with
            member _.Visit<'t> () =
                <@
                    match (% unwrap()) with
                    | null -> 0
                    | ts ->
                        let mutable agg = 0
                        for t in ts do
                            let th = (% genHash() ) t
                            agg <- (% Expr.lam2 combineHash) agg th
                        agg
                @> }

    | Shape.FSharpOption s ->
        s.Element.Accept { new ITypeVisitor<Expr<int>> with
            member _.Visit<'t> () =
                <@
                    match (% (unwrap() : Expr<'t option>)) with
                    | None -> 0
                    | Some t -> 
                        let th = (% genHash() ) t
                        (% Expr.lam2 combineHash) 1 th
                @> }

    | Shape.FSharpList s ->
        s.Element.Accept { new ITypeVisitor<Expr<int>> with
            member _.Visit<'t> () =
                <@
                    let mutable agg = 0
                    let mutable ts = (% (unwrap() : Expr<'t list>))
                    while not(List.isEmpty ts) do
                        let th = (% genHash() ) (List.head ts)
                        agg <- (% Expr.lam2 combineHash) agg th
                        ts <- List.tail ts

                    agg
                @> }

    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
        let mkElementHasher tuple =
            shape.Elements
            |> Array.map (fun e -> stageMemberHash e tuple)
            |> Array.map (fun eh agg -> combineHash eh agg)
            |> Expr.update ("agg", <@ 0 @>)

        <@
            let tuple = %expr
            (% Expr.lam mkElementHasher) tuple
        @>

    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
        let mkFieldHasher record =
            shape.Fields
            |> Array.map (fun e -> stageMemberHash e record)
            |> Array.map (fun eh agg -> combineHash eh agg)
            |> Expr.update ("agg", <@ 0 @>)
                    
        <@
            let record = %expr
            (% Expr.lam mkFieldHasher) record
        @>

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
        let stageUnionCaseHasher (union : Expr<'T>) (tag : Expr<int>)
                                 (case : ShapeFSharpUnionCase<'T>) =
            
            case.Fields
            |> Array.map (fun c -> stageMemberHash c union)
            |> Array.map (fun fh agg -> combineHash fh agg)
            |> Expr.update ("agg", tag)

        let stageUnionCaseHashers (u : Expr<'T>) (tag : Expr<int>) =
            shape.UnionCases
            |> Array.map (stageUnionCaseHasher u tag)
            |> Expr.switch tag

        <@
            let union = %expr
            let tag = (% Expr.lam shape.GetTagExpr) union
            (% Expr.lam2 stageUnionCaseHashers) union tag  
        @>

    | _ -> failwithf "Unsupported shape %O" typeof<'T>



// Compilation code

let mkHashCodeExpr<'T> () =
    let F self = 
        { new StagedGenerator1 with 
            member _.Generate<'T,'R> (e:Expr<'T>) : Expr<'R> =
                mkStagedHasher self e |> unbox }

    let gen = Expr.Y1 F
    gen.Generate<'T, int>
    |> Expr.lam
    |> Expr.cleanup

let mkHasher<'T> () = mkHashCodeExpr<'T>() |> QuotationEvaluator.Evaluate
let decompileHasher<'T> () = mkHashCodeExpr<'T>() |> decompile


// examples

let hasher1 = mkHasher<int list * string option>()

hasher1 ([1 .. 100], Some "42")

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

let hasher2 = mkHasher<Bar list>()

hasher2 [UA ; UC { A = 12 ; B = "test" }; UB "string" ]

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

type P = Z | S of P

let hasher3 = mkHasher<P>()

hasher3 (S (S (S (S (S Z)))))

decompileHasher<P> ()
//fun t -> 
//    let rec func t = 
//        let tag = t.Tag 
//        if tag = 0 then tag 
//        elif tag = 1 then 
//            let mutable agg = tag 
//            agg <- let h1 = func t.item in (h1 <<< 5) + h1 ||| agg
//            agg 
//        else invalidOp "invalid tag"
//
//    func t