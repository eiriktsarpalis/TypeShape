#if TYPESHAPE_EXPOSE
module TypeShape.Core.StagingExtensions
#else
// NB we don't want to leak the `TypeShape` namespace
// to the public API of the assembly
// so we use a top-level internal module
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

    /// <summary>
    ///     Staged bind combinator
    /// </summary>
    /// <param name="bname">Variable binding name.</param>
    /// <param name="bexpr">Bind expression.</param>
    /// <param name="cont">Continuation expression builder.</param>
    let bind (bname : string, bexpr : Expr<'T>) (cont : Expr<'T> -> Expr<'S>) : Expr<'S> =
        match bexpr with
        | Var _ | Value _ -> cont bexpr // do not bind if expression is already evaluated
        | _ ->
            let var = new Var(bname, typeof<'T>)
            Expr.Let(var, bexpr, cont (cast(Expr.Var var))) |> cast<'S>

    /// <summary>
    ///     Staged bind combinator for mutable variables
    /// </summary>
    /// <param name="bname">Variable binding name.</param>
    /// <param name="bexpr">Bind expression.</param>
    /// <param name="cont">Continuation expression builder accepting a variable getter and setter.</param>
    let bindMutable (bname : string, bexpr : Expr<'T>)
                    (cont : Expr<'T> -> (Expr<'T> -> Expr<unit>) -> Expr<'S>) : Expr<'S> =

        let v = new Var(bname, typeof<'T>, isMutable = true)
        let getter = Expr.Var v |> cast<'T>
        let setter (e:Expr<'T>) = Expr.VarSet(v, e) |> cast<unit>
        Expr.Let(v, bexpr, cont getter setter) |> cast<'S>  

    /// lambda abstraction for lifting staged expression computations
    /// into embeddable trees
    let lam (f : Expr<'T> -> Expr<'S>) : Expr<'T -> 'S> =
        let var = new Var("t", typeof<'T>)
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
    let seq (comps : Expr<unit> []) : Expr<unit> =
        match comps with
        | [||] -> <@ () @>
        | [|c|] -> c
        | _ ->
            Array.foldBack 
                (fun c s -> <@ %c ; %s @>) 
                comps.[..comps.Length - 2] comps.[comps.Length - 1]

    /// Expands a collection of folding computations into a statically
    /// expanded expression tree
    let fold (folder : Expr<'State> -> Expr<'T> -> Expr<'State>) 
                (varName : string, init : Expr<'State>) (inputs : Expr<'T> []) : Expr<'State> =

        if inputs.Length = 0 then init else
        bindMutable (varName, init)
            (fun getter setter ->
                let body = inputs |> Array.map (fun t -> setter (folder getter t)) |> seq
                <@ %body ; %getter @>)

    /// Expands a collection of state-updating staged computations
    /// into a expression tree
    let update (varName : string, init : Expr<'T>) (comps : (Expr<'T> -> Expr<'T>) []) : Expr<'T> =
        if comps.Length = 0 then init else
        bindMutable (varName, init)
            (fun getter setter -> 
                let unfolded = comps |> Array.map (fun c -> setter (c getter)) |> seq
                <@ %unfolded ; %getter @>)

    /// expands a collection of expressions so that they
    /// branch according to the tag expression provided
    let switch (tag : Expr<int>) (cases : Expr<'T> []) =
        if cases.Length = 0 then invalidArg "cases" "Must be non-empty"
        bind ("tag", tag) (fun tag ->
            let rec aux rest i =
                if i < 0 then rest
                else aux <@ if %tag = i then (% cases.[i]) else %rest @> (i - 1)

            aux <@ invalidOp "invalid tag" @> (cases.Length - 1))

    /// traverses an expression tree applying the transformation
    /// `(fun x y z .. -> M[x,y,z,..]) a b c => M[a,b,c,..]`,
    /// where a,b,c are variables or constants
    let unlambda (expr : Expr<'T>) =
        let (|AppLambdas|_|) (e : Expr) =
            // traverses the "App(App(App ... " part of the expression
            let rec gatherApps args e =
                match e with
                | Application(lhs, ((Var _ | Value _) as rhs)) -> gatherApps (rhs :: args) lhs
                | _ -> args, e

            // traverses the "Lambda(Lambda(Lambda ... " part of the expression
            let rec gatherLambdas args acc e =
                match e, args with
                | _, [] -> Some (acc, e)
                | Lambda(v, body), hd :: tl -> gatherLambdas tl ((v, hd) :: acc) body
                | _ -> None

            // performs substitution of each recovered var with corresponding value
            let rec substitute vars (body : Expr) =
                match vars with
                | [] -> body
                | (var, value) :: rest ->
                    let body2 = body.Substitute(function v when v = var -> Some value | _ -> None)
                    substitute rest body2

            match gatherApps [] e with
            | [], _ -> None
            | args, body ->
                match gatherLambdas args [] body with
                | None -> None
                | Some(vars, body) -> Some(substitute vars body)

        // traverse the full expression tree
        let rec aux e =
            match e with
            | AppLambdas reducedExpr -> aux reducedExpr
            | ShapeVar _ -> e
            | ShapeLambda(v,b) -> Expr.Lambda(v, aux b)
            | ShapeCombination(comb, args) -> RebuildShapeCombination(comb, List.map aux args)

        cast<'T> (aux expr)

    /// Performs the transformation
    /// `let x = y in M[x]` => `M[y]` where y is a constant or variable.
    let unlet (expr : Expr<'T>) : Expr<'T> =
        let rec aux e =
            match e with
            | Let(x, (Var _ | Value _ as e), body) when not x.IsMutable ->
                body.Substitute(function v when v = x -> Some e | _ -> None)
                |> aux
            | ShapeVar _ -> e
            | ShapeLambda(v, body) -> Expr.Lambda(v, aux body)
            | ShapeCombination(comb, args) -> RebuildShapeCombination(comb, List.map aux args)

        aux expr |> cast<'T>

    /// Optimizes away staging artifacts from expression tree
    let cleanup (expr : Expr<'T>) : Expr<'T> =
        expr |> unlambda |> unlet