module TypeShape.Core.NBE

open System
open System.Reflection
open System.Collections.Generic
open FSharp.Reflection
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape

// Expression tree denotation
type Sem =
    | VAR of Var * value:Sem option
    | LIT of obj * Type
    | LAM of Var * (Sem -> Sem)
    | LET of Var * Sem * Sem
    | TUPLE of Type * Sem list
    | RECORD of Type * Sem list
    | UNION of UnionCaseInfo * Sem list
    //| OPERATOR of MethodInfo * Sem list
    | SYN of shape:obj * Sem list

let rec reify (rvars : HashSet<Var>) (s : Sem) : Expr =
    match s with
    | VAR (v, _) -> 
        let _ = rvars.Add v
        Expr.Var v
    | LIT (o, t) -> Expr.Value(o, t)
    | LAM (v, l) -> Expr.Lambda(v, l (VAR (v, None)) |> reify rvars)
    | LET (v, b, k) ->
        let eb = reify rvars b
        let ek = reify rvars k
        if rvars.Contains v then Expr.Let(v, eb, ek)
        else ek

    | TUPLE (_,es) -> Expr.NewTuple(es |> List.map (reify rvars))
    | RECORD (t, fs) -> Expr.NewRecord(t, fs |> List.map (reify rvars))
    | UNION (uci, fs) -> Expr.NewUnionCase(uci, fs |> List.map (reify rvars))
    | SYN(shape, sparams) -> RebuildShapeCombination(shape, sparams |> List.map (reify rvars))

let rec isValue (s : Sem) =
    match s with
    | LIT _ | LAM _ -> true
    | VAR (_, v) -> v |> Option.forall isValue
    | SYN _ | LET _ -> false
    | TUPLE (_,fs) | RECORD (_,fs) | UNION(_,fs) -> fs |> List.forall isValue

//let isClosed (s : Sem) =
//    let rec aux (env : Set<Var>) (s : Sem) =
//        match s with
//        | VAR v -> env.Contains v
//        | LIT _ -> true
//        | LAM(v, f) -> aux (env.Add v) (f (VAR v))
//        | TUPLE fs
//        | RECORD(_,fs)
//        | UNION(_,fs) 
//        | SYN(_,fs) -> fs |> List.forall (aux env)
//    aux Set.empty s

//let isLiteral s = isValue s && isClosed s

let rec dereference (s : Sem) =
    match s with
    | VAR(_, Some s) -> dereference s
    | VAR _ | LIT _ | LAM _ | LET _ | SYN _ as s -> s
    | TUPLE  (t,fs) -> TUPLE(t, fs |> List.map dereference)
    | RECORD (t,fs) -> RECORD(t, fs |> List.map dereference)
    | UNION(uci,fs) -> UNION(uci, fs |> List.map dereference)

type Environment = Map<Var, Sem>

let rec meaning (env : Environment) (expr : Expr) : Sem =
    let mkLit (x : 'T) = LIT(x, typeof<'T>)

    let (|DR|) = dereference

    let fallback (env : Environment) e =
        match e with
        | ShapeVar v -> 
            match env.TryFind v with
            | Some (LIT _ | VAR _ as s) -> s
            | sopt -> VAR(v, sopt)
        | ShapeLambda(v, body) -> LAM(v, fun s -> meaning (env.Add(v, s)) body)
        | ShapeCombination(shape, args) ->
            let sargs = args |> List.map (meaning env)
            SYN(shape, sargs)
        
    match expr with
    | Value(o, t) -> LIT(o, t)
    | Application(f, g) ->
        match meaning env f with
        | DR (LAM (v, l)) ->
            match meaning env g with
            // (λ x. M) N ~> M[N/x]
            | LIT _ | VAR _ | LAM _ as s -> l s
            | s -> 
                // (λ x. M) N ~> let x = N in M
                LET(v, s, l (VAR(v, Some s)))
        | _ -> fallback env expr

    | Let(v, binding, body) when not v.IsMutable ->
        let (DR s) = meaning env binding
        if isValue s then
            let sk = meaning (env.Add(v, s)) body
            LET(v, s, sk)
        else fallback env expr

    | IfThenElse(cond, ifExpr, elseExpr) ->
        match meaning env cond with
        | DR (LIT(:? bool as ccond, _)) ->
            let branch = if ccond then ifExpr else elseExpr
            meaning env branch

        | _ -> fallback env expr

    | Sequential(left, right) ->
        match meaning env left with
        | s when isValue s -> meaning env right
        | _ -> fallback env expr

    // https://github.com/dotnet/fsharp/issues/7914
    | NewTuple ts when not expr.Type.IsValueType -> 
        TUPLE(expr.Type, ts |> List.map (meaning env))

    | TupleGet(tuple, i) ->
        match meaning env tuple with
        | DR (TUPLE (_, ts)) -> ts.[i]
        | _ -> fallback env expr

    | NewRecord(t, fs) -> RECORD(t, fs |> List.map (meaning env))
    | PropertyGet(Some e, prop, []) ->
        match meaning env e with
        | DR (RECORD(t,fs)) -> 
            match FSharpType.GetRecordFields(t, true) |> Array.tryFindIndex(fun p -> prop = p) with
            | Some i -> fs.[i]
            | None -> fallback env expr
        | DR (UNION(uci, fs)) ->
            match uci.GetFields() |> Array.tryFindIndex(fun p -> prop = p) with
            | Some i -> fs.[i]
            | None -> fallback env expr
        | _ -> fallback env expr

    | NewUnionCase(uci, fs) -> UNION(uci, fs |> List.map (meaning env))
    | UnionCaseTest(e, uci) ->
        match meaning env e with
        | DR (UNION(uci', _)) -> mkLit(uci = uci')
        | _ -> fallback env expr

    | TypeTest(e, t) ->
        match meaning env e with
        | DR (LIT(null,_)) -> mkLit(false)
        | DR (LIT(o,_)) -> mkLit(t.IsAssignableFrom(o.GetType()))
        | DR (TUPLE(tt,_)) -> mkLit(t = tt)
        | DR (RECORD(rt,_)) -> mkLit(t = rt)
        | DR (UNION(uci,_)) -> mkLit(uci.DeclaringType = t)
        | _ -> fallback env expr

    | SpecificCall <@ (|>) @> (None, _, [value; func])
    | SpecificCall <@ (<|) @> (None, _, [func; value]) -> meaning env (Expr.Application(func, value))

    | SpecificCall <@ ignore @> (None, _, [value]) -> meaning env (Expr.Sequential(value, Expr.Value(())))

    | Call(None, mi, args) when mi.DeclaringType.FullName = "Microsoft.FSharp.Core.Operators" ->
        let constArgs = 
            args 
            |> Seq.map (meaning env) 
            |> Seq.choose (function DR (LIT(o,t)) when t.IsValueType -> Some o | _ -> None)
            |> Seq.toArray

        if constArgs.Length = args.Length then
            let o = mi.Invoke(null, constArgs)
            LIT(o, expr.Type)
        else
            fallback env expr

    | _ -> fallback env expr

let nbe (e : Expr<'a>) : Expr<'a> = reify (new HashSet<Var>()) (meaning Map.empty e) |> Expr.Cast