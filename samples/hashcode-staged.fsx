#r "../bin/Release/net40/TypeShape.dll"
#r "../bin/Release/net40/Unquote.dll"
#r "../packages/FSharp.Quotations.Evaluator/lib/net40/FSharp.Quotations.Evaluator.dll"

open FSharp.Quotations
open FSharp.Quotations.Evaluator
open Swensen.Unquote
open TypeShape
open TypeShape_StagingExtensions

// Rank-2 encoding of staged hashcode generator
type StagedHashCodeGenerator =
    abstract Generate<'T> : Expr<'T> -> Expr<int>

// Variation of the Y combinator for generic staged hashode generators
let Y : (StagedHashCodeGenerator -> StagedHashCodeGenerator) -> StagedHashCodeGenerator =
    fun F ->
        let rec aux (stack : Var list) (e : Expr<'a>) : Expr<int> =
            match stack |> List.tryFind (fun v -> v.Type = typeof<'a -> int>) with
            | None ->
                // first time this type occurs in the stack, push fresh function variable
                // into stack and perform staged computation
                let selfVar = Var("func", typeof<'a -> int>)
                let tVar = Var("t", typeof<'a>)
                let nestedGen = mkRecursiveGen (selfVar :: stack)

                // generate the staged body for the given type
                let body = nestedGen.Generate (Expr.Cast<'a> (Expr.Var tVar))

                // check whether type has been called recursively in the staged expression tree
                if body.GetFreeVars() |> Seq.exists ((=) selfVar) then
                    // we are looking at a recursive type, wrap body inside a recursive function declaration
                    let lambda = Expr.Lambda(tVar, body)
                    let recExpr = Expr.Cast<'a -> int>(Expr.LetRecursive([selfVar, lambda], Expr.Var selfVar))
                    <@ (%recExpr) %e @>
                else
                    // not a recursive type, just replace input variable with input expression
                    Expr.Cast<_>(body.Substitute(function v when v = tVar -> Some (e :> _) | _ -> None))

            | Some self ->
                // we are already inside a recursive call, just invoke the recursive function argument
                let selfExpr = Expr.Cast<'a -> int>(Expr.Var self)
                <@ (%selfExpr) %e @>

        and mkRecursiveGen stack : StagedHashCodeGenerator = 
            F { new StagedHashCodeGenerator with member __.Generate e = aux stack e }

        mkRecursiveGen []

// The generic program itself

let rec mkStagedHasher() =
    Y (fun self -> { new StagedHashCodeGenerator with member __.Generate expr = mkStagedHasherAux self expr })

and private mkStagedHasherAux<'T> (self : StagedHashCodeGenerator) (expr : Expr<'T>) : Expr<int> =
    let unwrap () = unbox<Expr<'a>> expr
    let genHash () = Expr.lam (fun e -> self.Generate<'a> e)

    let combineHash (h1 : Expr<int>) (h2 : Expr<int>) =
        <@ let h1 = %h1 in let h2 = %h2 in ((h1 <<< 5) + h1) ||| h2 @>

    let stageMemberHash (shape : IShapeMember<'DeclaringType>) (dExpr : Expr<'DeclaringType>) =
        shape.Accept { new IMemberVisitor<'DeclaringType, Expr<int>> with
            member __.Visit (shape : ShapeMember<'DeclaringType, 'FieldType>) =
                let fExpr = shape.ProjectExpr dExpr
                self.Generate fExpr }

    match shapeof<'T> with
    | Shape.Unit -> <@ 0 @>
    | Shape.Bool -> <@ if (% unwrap() ) then 1 else 0 @>
    | Shape.Int32 -> <@ (% unwrap()) @>
    | Shape.Double -> <@ hash<double> (% unwrap()) @>
    | Shape.String -> <@ hash<string> (% unwrap()) @>
    | Shape.Array s when s.Rank = 1 ->
        s.Accept { new IArrayVisitor<Expr<int>> with
            member __.Visit<'t> _ =
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
        s.Accept { new IFSharpOptionVisitor<Expr<int>> with
            member __.Visit<'t> () =
                <@
                    match (% (unwrap() : Expr<'t option>)) with
                    | None -> 0
                    | Some t -> 
                        let th = (% genHash() ) t
                        (% Expr.lam2 combineHash) 1 th
                @> }

    | Shape.FSharpList s ->
        s.Accept { new IFSharpListVisitor<Expr<int>> with
            member __.Visit<'t> () =
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
        let stageUnionCaseHasher 
            (union : Expr<'T>) (tag : Expr<int>)
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
    fun e -> mkStagedHasher().Generate<'T> e
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