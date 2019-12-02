module TypeShape.Core.NBE

open System
open System.Collections.Generic
open System.Reflection
open FSharp.Reflection
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape

// Semantic interpretation of quotation trees
type Sem =
    | VAR of Var * value:Sem option
    | LIT of obj * Type
    | LAM of Var * (Sem -> Sem)
    | LET of Var * Sem * Sem
    | TUPLE of Type * Sem list
    | RECORD of Type * Sem list
    | UNION of UnionCaseInfo * Sem list
    // Standard arithmetic operator expression
    | OP of MethodInfo * Sem list
    // Embeds arbitrary syntactic trees
    | SYN of isPureNode:bool * shape:obj * Sem list

/// determines if expression is guaranteed to evaluate without side-effects
let rec isPure (s : Sem) =
    match s with
    | VAR (v,_) -> not v.IsMutable
    | LIT _ | LAM _ -> true
    | SYN(isPureNode = isPure) -> isPure
    | LET(_,b,k) -> isPure b && isPure k
    | TUPLE (_,fs) | RECORD (_,fs) | UNION(_,fs) | OP(_,fs) -> fs |> List.forall isPure

type Environment = Map<Var, Sem>

/// Maps syntactic trees to semantic expressions; apply optimizations as required
let rec meaning (env : Environment) (expr : Expr) : Sem =
    let mkLit (x : 'T) = LIT(x, typeof<'T>)

    let (|Deref|) s =
        match s with
        | VAR(_, Some s) -> s
        | _ -> s

    // trivial semantic mapping; use as fallback when no other optimizations can be applied
    let fallback (env : Environment) (expr : Expr) =
        match expr with
        | ShapeVar v -> 
            match env.TryFind v with
            | Some (LIT _ | VAR _ as s) -> s
            | sopt -> VAR(v, sopt)
        | ShapeLambda(v, body) -> LAM(v, fun s -> meaning (env.Add(v, s)) body)
        | ShapeCombination(shape, args) ->
            let sargs = args |> List.map (meaning env)
            let isPureNode =
                match expr with
                | IfThenElse _
                | TupleGet _
                | TypeTest _
                | UnionCaseTest _
                | LetRecursive _
                | QuoteRaw _
                | QuoteTyped _ -> true
                | PropertyGet(Some e, _, []) when 
                    FSharpType.IsRecord(e.Type, true) || 
                    FSharpType.IsUnion(e.Type, true) ||
                    FSharpType.IsTuple(e.Type) -> true
                | _ -> false

            SYN(isPureNode, shape, sargs)
        
    match expr with
    | Value(o, t) -> LIT(o, t)
    | Application(f, g) ->
        match meaning env f with
        | Deref (LAM (v, lam)) ->
            match meaning env g with
            // (λ x. M) N ~> M[N/x]
            | LIT _ | VAR _ | LAM _ as s -> lam s
            // (λ x. M) N ~> let x = N in M
            | s -> LET(v, s, lam (VAR(v, Some s)))
        | _ -> fallback env expr

    | Let(v, binding, body) when not v.IsMutable ->
        let (Deref s) = meaning env binding
        match meaning (env.Add(v, s)) body with
        | VAR(x, _) when x = v -> s // let x = N in x ~> N
        | sk -> LET(v, s, sk)

    | IfThenElse(cond, ifExpr, elseExpr) ->
        match meaning env cond with
        | Deref (LIT(:? bool as ccond, _)) ->
            // branch elimination
            let branch = if ccond then ifExpr else elseExpr
            meaning env branch
        | _ -> fallback env expr

    | Sequential(left, right) ->
        match meaning env left with
        | ls when isPure ls -> 
            match meaning env right with
            | rs when isPure rs -> mkLit ()
            | rs -> rs
        | _ -> fallback env expr

    // Tuple introduction & elimination [https://github.com/dotnet/fsharp/issues/7914]
    | NewTuple ts when not expr.Type.IsValueType -> TUPLE(expr.Type, ts |> List.map (meaning env))
    | TupleGet(tuple, i) ->
        match meaning env tuple with
        | Deref (TUPLE (_, ts)) -> ts.[i]
        | _ -> fallback env expr

    // Record introduction & elimination
    | NewRecord(t, fs) -> RECORD(t, fs |> List.map (meaning env))
    | PropertyGet(Some e, prop, []) ->
        match meaning env e with
        | Deref (RECORD(t,fs)) -> 
            match FSharpType.GetRecordFields(t, true) |> Array.tryFindIndex(fun p -> prop = p) with
            | Some i -> fs.[i]
            | None -> fallback env expr
        | Deref (UNION(uci, fs)) ->
            match uci.GetFields() |> Array.tryFindIndex(fun p -> prop = p) with
            | Some i -> fs.[i]
            | None -> fallback env expr
        | _ -> fallback env expr

    // Union introduction & elimination
    | NewUnionCase(uci, fs) -> UNION(uci, fs |> List.map (meaning env))
    | UnionCaseTest(e, uci) ->
        match meaning env e with
        | Deref (UNION(uci', _)) -> mkLit(uci = uci')
        | _ -> fallback env expr

    | SpecificCall <@ (|>) @> (None, _, [value; func])
    | SpecificCall <@ (<|) @> (None, _, [func; value]) -> meaning env (Expr.Application(func, value))

    | SpecificCall <@ fst @> (None, _, [t]) -> meaning env (Expr.TupleGet(t, 0))
    | SpecificCall <@ snd @> (None, _, [t]) -> meaning env (Expr.TupleGet(t, 1))
    | SpecificCall <@ ignore @> (None, _, [value]) -> meaning env (Expr.Sequential(value, Expr.Value(())))

    | Call(None, mi, args) when mi.DeclaringType.FullName = "Microsoft.FSharp.Core.Operators" && mi.Name.StartsWith "op_" ->
        let sargs = args |> List.map (meaning env)
        let literals = sargs |> Seq.choose (function (Deref (LIT(o,_))) -> Some o | _ -> None) |> Seq.toArray
        if literals.Length = sargs.Length then
            // evaluate constant expressions
            try LIT(mi.Invoke(null, literals), expr.Type)
            with :? TargetException as e when (e.InnerException :? NotSupportedException) ->
                // "Dynamic invocation of operator is not supported"
                OP(mi, sargs)
        else
            OP(mi, sargs)

    | _ -> fallback env expr

/// Maps semantic expressions back to a syntactic representation
let reify (s : Sem) : Expr =
    let rec aux (refdVars : HashSet<Var>) s =
        match s with
        | VAR (v, _) -> let _ = refdVars.Add v in Expr.Var v
        | LIT (o, t) -> Expr.Value(o, t)
        | LAM (v, lam) -> Expr.Lambda(v, lam (VAR (v, None)) |> aux refdVars)
        | LET (v, b, k) ->
            let ek = aux refdVars k
            if isPure b && not (refdVars.Contains v) then ek
            else
                let eb = aux refdVars b
                Expr.Let(v, eb, ek)

        | TUPLE (_,fs) -> Expr.NewTuple(fs |> List.map (aux refdVars))
        | RECORD (t, fs) -> Expr.NewRecord(t, fs |> List.map (aux refdVars))
        | UNION (uci, fs) -> Expr.NewUnionCase(uci, fs |> List.map (aux refdVars))
        | OP(mI, args) -> Expr.Call(mI, args |> List.map (aux refdVars))
        | SYN(_, shape, sparams) -> RebuildShapeCombination(shape, sparams |> List.map (aux refdVars))

    aux (HashSet()) s

/// Apply normalization-by-evaluation to quotation tree
let nbe (e : Expr<'a>) : Expr<'a> = e |> meaning Map.empty |> reify |> Expr.Cast