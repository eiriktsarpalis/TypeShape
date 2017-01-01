#if TYPESHAPE_EXPOSE
module TypeShape_StagingExtensions
#else
module internal TypeShape_StagingExtensions
#endif

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape

[<RequireQualifiedAccess>]
module Expr =

    /// Converts an untyped expression into its typed counterpart
    let cast<'T> e = Expr.Cast<'T> e

    /// lambda abstraction for lifting staged expression computations
    /// into embeddable trees
    let lam (f : Expr<'T1> -> Expr<'S>) : Expr<'T1 -> 'S> =
        let var = new Var("t", typeof<'T1>)
        Expr.Lambda(var,  f (cast(Expr.Var var)))
        |> cast

    /// lambda abstraction for lifting staged expression computations
    /// into embeddable trees
    let lam2 (f : Expr<'T1> -> Expr<'T2> -> Expr<'S>) : Expr<'T1 -> 'T2 -> 'S> =
        let t1v = new Var("t1", typeof<'T1>)
        let t2v = new Var("t2", typeof<'T2>)
        Expr.Lambda(t1v,
            Expr.Lambda(t2v,
                f (cast(Expr.Var t1v))
                  (cast(Expr.Var t2v))))
        |> cast

    /// lambda abstraction for lifting staged expression computations
    /// into embeddable trees
    let lam3 (f : Expr<'T1> -> Expr<'T2> -> Expr<'T3> -> Expr<'S>) : Expr<'T1 -> 'T2 -> 'T3 -> 'S> =
        let t1v = new Var("t1", typeof<'T1>)
        let t2v = new Var("t2", typeof<'T2>)
        let t3v = new Var("t3", typeof<'T3>)
        Expr.Lambda(t1v,
            Expr.Lambda(t2v,
                Expr.Lambda(t3v,
                    f (cast(Expr.Var t1v))
                      (cast(Expr.Var t2v))
                      (cast(Expr.Var t3v)))))
        |> cast

    /// expands a collection of boolean expressions
    /// into a sequence of inlined &&'s
    let forall (fs : Expr<bool> []) =
        match Array.toList fs with
        | [] -> <@ true @>
        | hd :: tl -> tl |> List.fold (fun s f -> <@ %s && %f @>) hd

    /// expands a collection of computations
    /// into a sequenced expression
    let iter (comps : Expr<unit> []) : Expr<unit> =
        match Array.toList comps with
        | [] -> <@ () @>
        | hd :: tl -> tl |> List.fold (fun s f -> <@ %s ; %f @>) hd

    /// Expands a collection of state-updating staged computations
    /// into a expression tree
    let update (init : Expr<'T>) (comps : (Expr<'T> -> Expr<'T>) []) : Expr<'T> =
        let state = Var("state", typeof<'T>, isMutable = true)
        let getVar = Expr.Var(state) |> cast<'T>
        let updateComps =
            comps
            |> Array.map (fun c -> Expr.VarSet(state, c getVar) |> cast<unit>)
            |> iter

        Expr.Let(state, init, Expr.Sequential(updateComps, getVar))
        |> cast<'T>

    /// expands a collection of expressions so that they
    /// branch according to the tag expression provided
    let switch (tag : Expr<int>) (cases : Expr<'T> []) =
        let rec aux rest i =
            if i < 0 then rest
            else aux <@ if %tag = i then (% cases.[i]) else %rest @> (i - 1)

        aux <@ invalidOp "invalid tag" @> (cases.Length - 1)

    /// traverses an expression tree applying the transformation
    /// `(fun x y z .. -> M[x,y,z,..]) a b c => M[a,b,c,..]`,
    /// where a,b,c are variables or constants
    let unlambda (expr : Expr<'T>) =
        let (|ValidArg|_|) = function (Var _ | Value _) as e -> Some e | _ -> None
        let (|AppLambdas|_|) (e : Expr) =
            // traverses the "App(App(App ... " part of the expression
            let rec gatherApps args e =
                match e with
                | Application(lhs, ValidArg rhs) -> gatherApps (rhs :: args) lhs
                | _ -> args, e

            // traverses the "Lambda(Lambda(Lambda ... " part of the expression
            let rec gatherLambdas args acc e =
                match args, e with
                | [], _ -> Some (acc, e)
                | hd :: tl, Lambda(v, body) -> gatherLambdas tl ((v, hd) :: acc) body
                | _ -> None

            match gatherApps [] e with
            | [], _ -> None
            | args, body -> 
                match gatherLambdas args [] body with
                | None -> None
                | Some(vars, body) ->
                    let sub (body:Expr) (var:Var, value:Expr) =
                        body.Substitute(function v when v = var -> Some value | _ -> None)
                    List.fold sub body vars |> Some

        let rec aux e =
            match e with
            | AppLambdas reducedExpr -> aux reducedExpr
            | ShapeVar _ -> e
            | ShapeLambda(v,b) -> Expr.Lambda(v, aux b)
            | ShapeCombination(comb, args) -> RebuildShapeCombination(comb, List.map aux args)

        cast<'T> (aux expr)