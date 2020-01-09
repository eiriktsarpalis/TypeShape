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
    | LAM of Var * Sem * (Sem -> Sem)
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
    | LIT (_, t) -> t
    | LAM (v, s, _) -> FSharpType.MakeFunctionType(v.Type, getType false s)
    | LET (_,_,k) -> getType reflect k
    | UPCAST (s,t) -> if reflect then getType reflect s else t
    | TUPLE (t,_) -> t
    | RECORD (t,_) -> t
    | UNION(uci,_) -> uci.DeclaringType
    | OP(mi,_) -> mi.ReturnType
    | SYN(expr = e) -> e.Type

/// determines if the two types are in a subtype relationship
let rec isSubtypeOf (iface : Type) (ty : Type) =
    let proj (t : Type) = t.Assembly, t.Namespace, t.Name, t.MetadataToken
    if iface.IsAssignableFrom ty then true
    elif ty.GetInterfaces() |> Array.exists(fun if0 -> proj if0 = proj iface) then true
    else
        match ty.BaseType with
        | null -> false
        | bt -> isSubtypeOf iface bt

let rec containsReference (v : Var) (s : Sem) =
    match s with
    | VAR (w, _) -> v = w
    | LIT _ -> false
    | LAM (_, b, _) -> containsReference v b
    | LET (_, b, k) -> containsReference v b || containsReference v k
    | UPCAST (s, _) -> containsReference v s
    | TUPLE (_, args)
    | RECORD (_, args)
    | UNION (_, args)
    | OP (_, args)
    | SYN (_, _, _, args) -> args |> List.exists (containsReference v)

type Environment = Map<Var, Sem>

/// Maps syntactic trees to semantic expressions; apply optimizations as required
let rec meaning (env : Environment) (expr : Expr) : Sem =
    let mkLit (x : 'T) = LIT(x, typeof<'T>)

    let rec (|Deref|) s =
        match s with
        | VAR(_, Some s) -> s
        | _ -> s

    let mkLet v sbind skont =
        // if binding is pure and variable not referenced in kont, eliminate the let
        if isPure sbind && skont |> containsReference v |> not then skont
        else LET(v, sbind, skont)

    // fallback mapping: use trivial expression node embedding
    let fallback (env : Environment) (expr : Expr) =
        match expr with
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

        | ShapeVar _ | ShapeLambda _ -> failwith "internal error"
        
    match expr with
    | Value(o, t) -> LIT(o, t)
    | Var v -> 
        match env.TryFind v with
        | Some (LIT _ | VAR _ as s) -> s
        | sopt -> VAR(v, sopt)

    | Lambda(v, body) -> 
        let slam s = meaning (env.Add(v, s)) body
        LAM(v, slam (VAR(v, None)), slam)

    | Application(f, g) ->
        match meaning env f with
        | Deref (LAM (v, _, lam)) ->
            match meaning env g with
            // (λ x. M) N ~> M[N/x]
            | LIT _ | VAR _ | LAM _ as s -> lam s
            // (λ x. M) N ~> let x = N in M
            | sbind -> mkLet v sbind (lam (VAR(v, Some sbind)))

        | _ -> fallback env expr

    | Let(v, bind, kont) when not v.IsMutable ->
        let sbind = meaning env bind
        match meaning (env.Add(v, sbind)) kont with
        // let x = N in x ~> N
        | VAR(x, _) when x = v -> sbind 
        // let x = expr in M ~> M
        | skont -> mkLet v sbind skont

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
            if isSubtypeOf t st then Some true
            elif isSubtypeOf st t then None
            else Some false

        match staticTypeTestResult with
        | Some r -> meaning env (Expr.Sequential(e, Expr.Value r))
        | None -> fallback env expr

    | Coerce(e, t) ->
        let (s | UPCAST(s,_)) = meaning env e
        let st = getType true s
        if t = st then
            // UPCAST elimination: remove if expression type matches the reflected type
            let rec tryDeref s =
                match s with
                | LIT _ -> Some s
                | UPCAST(s, _) -> tryDeref s
                | VAR(_, Some v) ->
                    match tryDeref v with
                    | Some _ as r -> r
                    | None -> Some s

                // do not deref if expr is method
                // or other side-effectful operation
                | _ -> None

            match tryDeref s with
            | Some s when getType false s = t -> s
            | _ -> UPCAST(s, t)

        elif isSubtypeOf t st then UPCAST(s, t)
        else fallback env expr

    | SpecificCall <@ (=) @> (None, _, ([value; Value(null,_)] | [Value(null,_) ; value]))
    | SpecificCall <@ isNull @> (None, _, ([value])) ->
        match meaning env value with
        | Deref (LIT (x,_)) -> mkLit(obj.ReferenceEquals(x, null))
        | _ -> fallback env expr

    | SpecificCall <@ (<>) @> (None, _, ([value; Value(null,_)] | [Value(null,_) ; value])) ->
        match meaning env value with
        | Deref (LIT (x,_)) -> mkLit(not <| obj.ReferenceEquals(x, null))
        | _ -> fallback env expr

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
let rec reify (s : Sem) : Expr =
    match s with
    | VAR (v, _) -> Expr.Var v
    | LIT (o, t) -> Expr.Value(o, t)
    | LAM (v, b, _) -> Expr.Lambda(v, reify b)
    | LET (v, b, k) -> Expr.Let(v, reify b, reify k)
    | UPCAST (s, t) -> Expr.Coerce(reify s, t)
    | TUPLE (_, fs) -> Expr.NewTuple(fs |> List.map reify)
    | RECORD (t, fs) -> Expr.NewRecord(t, fs |> List.map reify)
    | UNION (uci, fs) -> Expr.NewUnionCase(uci, fs |> List.map reify)
    | OP(mI, args) -> Expr.Call(mI, args |> List.map reify)
    | SYN(_, _, shape, sparams) -> RebuildShapeCombination(shape, sparams |> List.map reify)

/// Apply normalization-by-evaluation to quotation tree
let nbeUntyped (e : Expr) : Expr = e |> meaning Map.empty |> reify

/// Apply normalization-by-evaluation to quotation tree
let nbe (e : Expr<'a>) : Expr<'a> = e |> nbeUntyped |> Expr.Cast