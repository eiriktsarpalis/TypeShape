module TypeShape.Core.NBE

open System
open System.Reflection
open FSharp.Reflection
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape

#nowarn "1204"

// Semantic interpretation of quotation trees
type Sem =
    | VAR of Var * value:Sem option
    | LIT of obj * Type
    | LAM of Var * (Sem -> Sem)
    | LET of Var * Sem * Sem
    | TUPLE of Type * Sem list
    | RECORD of Type * Sem list
    | UNION of UnionCaseInfo * Sem list
    | UPCAST of Sem * Type
    // Standard arithmetic operator expression
    | OP of MethodInfo * Sem list
    // Embeds arbitrary syntactic trees
    | SYN of expr:Expr * isPureNode:bool * shape:obj * args:Sem list

/// determines if expression is guaranteed to evaluate without side-effects
let rec isPure (s : Sem) =
    match s with
    | VAR (v,_) -> not v.IsMutable
    | LIT _ | LAM _ -> true
    | LET(_,b,k) -> isPure b && isPure k
    | UPCAST (s,_) -> isPure s
    | TUPLE (_,fs) | RECORD (_,fs) | UNION(_,fs) | OP(_,fs) -> fs |> List.forall isPure
    | SYN(isPureNode = isPureNode; args = args) -> isPureNode && args |> List.forall isPure

let rec getType reflect (s : Sem) =
    match s with
    | VAR (v, None) -> v.Type
    | VAR (_, Some s) -> getType reflect s
    | LIT (null, t) -> t
    | LIT (o, _) -> o.GetType()
    | LAM (v, f) -> FSharpType.MakeFunctionType(v.Type, getType reflect (f (VAR(v, None))))
    | LET (_,_,k) -> getType reflect k
    | UPCAST (s,t) -> if reflect then getType reflect s else t
    | TUPLE (t,_) -> t
    | RECORD (t,_) -> t
    | UNION(uci,_) -> uci.DeclaringType
    | OP(mi,_) -> mi.ReturnType
    | SYN(expr = e) -> e.Type

// correctly resolves if type is assignable to interface
let rec isAssignableFrom (iface : Type) (ty : Type) =
    let proj (t : Type) = t.Assembly, t.Namespace, t.Name, t.MetadataToken
    if iface.IsAssignableFrom ty then true
    elif ty.GetInterfaces() |> Array.exists(fun if0 -> proj if0 = proj iface) then true
    else
        match ty.BaseType with
        | null -> false
        | bt -> isAssignableFrom iface bt

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

            SYN(expr, isPureNode, shape, sargs)
        
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
        let s = meaning env binding
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
        | ls when isPure ls -> meaning env right
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

    | TypeTest(e, t) ->
        let s = meaning env e
        let st = getType true s
        let staticTypeTestResult =
            if isAssignableFrom t st then Some true
            elif isAssignableFrom st t then None
            else Some false

        match staticTypeTestResult with
        | Some r -> meaning env (Expr.Sequential(e, Expr.Value r))
        | None -> fallback env expr

    | Coerce(e, t) ->
        let (s | UPCAST(s,_)) = meaning env e
        let st = getType true s
        if t = st then
            // UPCAST elimination: remove if expression type matches reflected type
            let rec tryInline s =
                match s with
                | VAR (_, Some s0) -> 
                    match tryInline s0 with
                    | None -> Some s
                    | Some _ as r -> r

                | UPCAST(s,_) -> tryInline s
                | LIT _ as s -> Some s
                | _ -> None

            match tryInline s with
            | Some s when getType false s = t -> s
            | _ -> UPCAST(s, t)

        elif isAssignableFrom t (getType true s) then UPCAST(s, t)
        else fallback env expr

    | SpecificCall <@ (=) @> (None, _, ([value; Value(null,_)] | [Value(null,_) ; value]))
    | SpecificCall <@ isNull @> (None, _, ([value])) ->
        match meaning env value with
        | Deref (LIT (x,_)) -> mkLit(obj.ReferenceEquals(x, null))
        | _ -> fallback env value

    | SpecificCall <@ (<>) @> (None, _, ([value; Value(null,_)] | [Value(null,_) ; value])) ->
        match meaning env value with
        | Deref (LIT (x,_)) -> mkLit(not <| obj.ReferenceEquals(x, null))
        | _ -> fallback env value

    | SpecificCall <@ (|>) @> (None, _, [value; func])
    | SpecificCall <@ (<|) @> (None, _, [func; value]) -> meaning env (Expr.Application(func, value))

    | SpecificCall <@ fst @> (None, _, [t]) -> meaning env (Expr.TupleGet(t, 0))
    | SpecificCall <@ snd @> (None, _, [t]) -> meaning env (Expr.TupleGet(t, 1))
    | SpecificCall <@ ignore @> (None, _, [value]) -> meaning env (Expr.Sequential(value, Expr.Value(())))

    | SpecificCall <@ box @> (None, _, [value])
    | SpecificCall <@ LanguagePrimitives.IntrinsicFunctions.UnboxGeneric @> (None, _, [value])
    | SpecificCall <@ LanguagePrimitives.IntrinsicFunctions.UnboxFast @> (None, _, [value])
    | SpecificCall <@ unbox @> (None, _, [value]) -> meaning env (Expr.Coerce(value, expr.Type))

    | Call(None, mi, args) when mi.DeclaringType.FullName = "Microsoft.FSharp.Core.Operators" && mi.Name.StartsWith "op_" ->
        let sargs = args |> List.map (meaning env)
        let literals = sargs |> Seq.choose (function (Deref (LIT(o,_))) -> Some o | _ -> None) |> Seq.toArray
        if literals.Length = sargs.Length then
            // evaluate constant expressions
            try LIT(mi.Invoke(null, literals), expr.Type)
            with :? TargetInvocationException as e when (e.InnerException :? NotSupportedException) ->
                // "Dynamic invocation of operator is not supported"
                OP(mi, sargs)
        else
            OP(mi, sargs)

    | _ -> fallback env expr

/// Maps semantic expressions back to a syntactic representation
let reify (s : Sem) : Expr =
    let rec aux s =
        let foldMap ss =
            let refss, es = ss |> List.map aux |> List.unzip
            Set.unionMany refss, es

        match s with
        | VAR (v, _) -> Set.singleton v, Expr.Var v
        | LIT (o, t) -> Set.empty, Expr.Value(o, t)
        | LAM (v, lam) -> 
            let refs, ebody = lam (VAR (v, None)) |> aux
            Set.remove v refs, Expr.Lambda(v, ebody)
        | LET (v, b, k) ->
            let krefs, ek = aux k
            if isPure b && not (krefs.Contains v) then krefs, ek
            else
                let brefs, eb = aux b
                let refs = krefs |> Set.union brefs |> Set.remove v
                refs, Expr.Let(v, eb, ek)

        | UPCAST (s, t) -> let scope, es = aux s in scope, Expr.Coerce(es, t)
        | TUPLE (_,fs) -> let scope, es = foldMap fs in scope, Expr.NewTuple(es)
        | RECORD (t, fs) -> let scope, es = foldMap fs in scope, Expr.NewRecord(t, es)
        | UNION (uci, fs) -> let scope, es = foldMap fs in scope, Expr.NewUnionCase(uci, es)
        | OP(mI, args) -> let scope, es = foldMap args in scope, Expr.Call(mI, es)
        | SYN(_, _, shape, sparams) -> let scope, es = foldMap sparams in scope, RebuildShapeCombination(shape, es)

    aux s |> snd

/// Apply normalization-by-evaluation to quotation tree
let nbeUntyped (e : Expr) : Expr = e |> meaning Map.empty |> reify

/// Apply normalization-by-evaluation to quotation tree
let nbe (e : Expr<'a>) : Expr<'a> = e |> nbeUntyped |> Expr.Cast