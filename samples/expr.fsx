#r "../src/TypeShape/bin/Release/net45/TypeShape.dll"

open System
open System.Collections.Generic
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape
open TypeShape.Core

type CompiledExpr<'T> = Environment -> 'T

and Environment private (index : Map<Var, obj ref>) =
    new () = new Environment(Map.empty)
    member __.NewVar(v : Var, value : obj) = new Environment(Map.add v (ref value) index)
    member __.GetVar(v : Var) = index.[v].Value
    member __.UpdateVar(v : Var, value : obj) = index.[v] := value

    member __.Splice(e : Expr) =
        e.Substitute(fun var ->
            match index.TryFind var with
            | None -> None
            | Some value -> Some(Expr.ValueWithName(value.Value, var.Type, var.Name)))   


let rec meaning<'T> (expr : Expr<'T>) : CompiledExpr<'T> =
    let EQ(f : CompiledExpr<'a>) = unbox<CompiledExpr<'T>> f
    let cast (e : Expr) = Expr.Cast<_> e

    let meaningUntyped (e : Expr) =
        TypeShape.Create(e.Type).Accept {
            new ITypeShapeVisitor<CompiledExpr<obj>> with
                member __.Visit<'t>() =
                    let ce = meaning<'t>(cast e)
                    fun env -> ce env :> obj
        }

    let mkMemberWriter (s : IShapeWriteMember<'T>) (e : Expr) =
        s.Accept { new IWriteMemberVisitor<'T, CompiledExpr<'T -> 'T>> with
            member __.Visit(s : ShapeWriteMember<'T, 'Elem>) =
                let cElem = meaning<'Elem> (cast e)
                fun env t -> let e = cElem env in s.Inject t e }

    let mkProjection (s : IShapeMember) (value : Expr) =
        TypeShape.Create(value.Type).Accept { 
            new ITypeShapeVisitor<CompiledExpr<'T>> with
                member __.Visit<'Value> () =
                    let s = s :?> ShapeMember<'Value, 'T>
                    let cvalue = meaning<'Value> (cast value)
                    fun env -> let v = cvalue env in s.Project v }

    match expr with
    | Value(:? 'T as t, _)
    | ValueWithName(:? 'T as t, _, _) -> fun _ -> t
    | Var var -> fun env -> env.GetVar var :?> 'T

    | Application(func, arg) ->
        let argShape = TypeShape.Create arg.Type
        argShape.Accept { new ITypeShapeVisitor<CompiledExpr<'T>> with
            member __.Visit<'Arg>() =
                let cfunc = meaning<'Arg -> 'T> (cast func)
                let carg = meaning<'Arg> (cast arg)
                EQ(fun env -> (cfunc env) (carg env)) }

    | Lambda(var, body) ->
        match shapeof<'T> with
        | Shape.FSharpFunc s ->
            s.Accept { new IFSharpFuncVisitor<CompiledExpr<'T>> with
                member __.Visit<'Dom, 'Cod> () = 
                    let cbody = meaning<'Cod> (cast body)
                    EQ(fun env (v : 'Dom) -> let env' = env.NewVar(var, v) in cbody env') }

        | _ -> failwith "internal error"

    | Let(var, bind, cont) ->
        let vShape = TypeShape.Create var.Type
        vShape.Accept { new ITypeShapeVisitor<CompiledExpr<'T>> with
            member __.Visit<'Var>() = 
                let cbind = meaning<'Var> (cast bind)
                let ccont = meaning<'T> (cast cont)
                fun env -> 
                    let v = cbind env
                    let env' = env.NewVar(var, v)
                    ccont env'
                |> EQ }

    | IfThenElse(cond, left, right) ->
        let ccond = meaning<bool> (cast cond)
        let cleft = meaning<'T> (cast left)
        let cright = meaning<'T> (cast right)
        fun env -> if ccond env then cleft env else cright env

    | WhileLoop(cond, body) ->
        let ccond = meaning<bool> (cast cond)
        let cbody = meaning<unit> (cast body)
        EQ(fun env -> while ccond env do cbody env)

    | ForIntegerRangeLoop(var, lower, upper, body) ->
        let clower = meaning<int> (cast lower)
        let cupper = meaning<int> (cast upper)
        let cbody = meaning<unit> (cast body)
        EQ(fun env ->
            let env' = env.NewVar(var, null)
            for i = clower env to cupper env do
                env'.UpdateVar(var, i)
                cbody env')

    | TryWith(body,_,_,ev,handler) ->
        let cbody = meaning<'T> (cast body)
        let chandler = meaning<'T> (cast handler)
        fun env ->
            try cbody env
            with e -> let env' = env.NewVar(ev, e) in chandler env'

    | TryFinally(body, finalizer) ->
        let ccond = meaning<'T> (cast body)
        let cfinalizer = meaning<unit> (cast finalizer)
        fun env -> try ccond env finally cfinalizer env

    | Sequential(left, right) when left.Type = typeof<unit> ->
        let cleft = meaning<unit> (cast left)
        let cright = meaning<'T> (cast right)
        fun env -> cleft env ; cright env

    | SpecificCall <@ (+) @> (None, _, [left; right]) when typeof<'T> = typeof<int> ->
        let cleft = meaning<int> (cast left)
        let cright = meaning<int> (cast right)
        EQ(fun env -> cleft env + cright env)

    | SpecificCall <@ (-) @> (None, _, [left; right]) when typeof<'T> = typeof<int> ->
        let cleft = meaning<int> (cast left)
        let cright = meaning<int> (cast right)
        EQ(fun env -> cleft env - cright env)

    | SpecificCall <@ (*) @> (None, _, [left; right]) when typeof<'T> = typeof<int> ->
        let cleft = meaning<int> (cast left)
        let cright = meaning<int> (cast right)
        EQ(fun env -> cleft env * cright env)

    | SpecificCall <@ (=) @> (None, _, [left; right]) ->
        match TypeShape.Create left.Type with
        | Shape.Equality s ->
            s.Accept { new IEqualityVisitor<CompiledExpr<'T>> with
                member __.Visit<'a when 'a : equality>() =
                    let cleft = meaning<'a> (cast left)
                    let cright = meaning<'a> (cast right)
                    EQ(fun env -> cleft env = cright env) }

        | _ -> failwith "internal error"

    | SpecificCall <@ not @> (None, _, [pred]) ->
        let cpred = meaning<bool> (cast pred)
        EQ(not << cpred)

    | SpecificCall <@ (&&) @> (None, _, [left; right]) ->
        let cleft = meaning<bool> (cast left)
        let cright = meaning<bool> (cast right)
        EQ(fun env -> cleft env && cright env)

    | SpecificCall <@ (||) @> (None, _, [left; right]) ->
        let cleft = meaning<bool> (cast left)
        let cright = meaning<bool> (cast right)
        EQ(fun env -> cleft env || cright env)

    | NewTuple exprs ->
        match shapeof<'T> with
        | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
            let celems =
                Seq.map2 mkMemberWriter shape.Elements exprs
                |> Seq.toArray

            fun env ->
                let mutable tuple = shape.CreateUninitialized()
                for ce in celems do tuple <- ce env tuple
                tuple

        | _ -> failwith "internal error"

    | TupleGet (tuple, index) ->
        match TypeShape.Create tuple.Type with
        | Shape.Tuple s -> mkProjection s.Elements.[index] tuple
        | _ -> failwith "internal error"

    | NewRecord (_, args) ->
        match shapeof<'T> with
        | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
            let cfields =
                Seq.map2 mkMemberWriter shape.Fields args
                |> Seq.toArray

            fun env ->
                let mutable record = shape.CreateUninitialized()
                for cf in cfields do record <- cf env record
                record

        | _ -> failwith "internal error"

    | NewUnionCase(uci, args) ->
        match shapeof<'T> with
        | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
            let case = shape.UnionCases.[uci.Tag]
            let ufields =
                Seq.map2 mkMemberWriter case.Fields args
                |> Seq.toArray

            fun env ->
                let mutable union = case.CreateUninitialized()
                for uf in ufields do union <- uf env union
                union

        | _ -> failwith "internal error"

    | UnionCaseTest(matchValue, uci) ->
        match TypeShape.Create uci.DeclaringType with
        | Shape.FSharpUnion s ->
            s.Accept { new IFSharpUnionVisitor<CompiledExpr<'T>> with
                member __.Visit<'Union> (shape : ShapeFSharpUnion<'Union>) =
                    let tag = uci.Tag
                    let cmv = meaning<'Union> (cast matchValue)
                    EQ(fun env -> shape.GetTag(cmv env) = tag) }

        | _ -> failwith "internal error"

    | LetRecursive(recBindings, cont) ->
        let mkRecBinding (var : Var, body : Expr) =
            let s = TypeShape.Create var.Type
            s.Accept { new ITypeShapeVisitor<Var * (Environment -> unit)> with
                member __.Visit<'Body>() =
                    let cbody = meaning<'Body> (cast body)
                    var, fun env -> env.UpdateVar(var, cbody env) }

        let vars, bindings = 
            recBindings 
            |> Seq.map mkRecBinding 
            |> Seq.toArray
            |> Array.unzip

        let ccont = meaning<'T> (cast cont)

        fun env ->
            let env' = (env, vars) ||> Array.fold (fun e v -> e.NewVar(v,null))
            for binding in bindings do binding env'
            ccont env'

    | VarSet(var, expr) ->
        TypeShape.Create(expr.Type).Accept {
          new ITypeShapeVisitor<CompiledExpr<'T>> with
            member __.Visit<'t>() =
                let cexpr = meaning<'t>(cast expr)
                EQ(fun env -> env.UpdateVar(var, cexpr env))
        }

    | Coerce(target, _) ->
        TypeShape.Create(target.Type).Accept {
          new ITypeShapeVisitor<CompiledExpr<'T>> with
            member __.Visit<'t>() =
                let cexpr = meaning<'t>(cast expr)
                fun env -> cexpr env |> unbox<'T>
        }

    | TypeTest(target, ty) ->
        TypeShape.Create(target.Type).Accept {
          new ITypeShapeVisitor<CompiledExpr<'T>> with
            member __.Visit<'t>() =
              let ctarget = meaning<'t>(cast expr)
              TypeShape.Create(ty).Accept {
                new ITypeShapeVisitor<CompiledExpr<'T>> with
                  member __.Visit<'s>() =
                    EQ(fun env -> box (ctarget env) :? 's)
              }
        }

    | QuoteRaw q -> EQ(fun env -> env.Splice q)
    | QuoteTyped q ->
        TypeShape.Create(q.Type).Accept {
            new ITypeShapeVisitor<CompiledExpr<'T>> with
                member __.Visit<'t> () = // 'T = Expr<'t>
                    EQ(fun env -> Expr.Cast<'t> (env.Splice q))
        }

    | Call(obj,mI,args) ->
        let cobj = obj |> Option.map meaningUntyped
        let cargs = args |> Seq.map meaningUntyped |> Seq.toArray
        fun env ->
            let obj = match cobj with Some co -> co env | None -> null
            let args = cargs |> Array.map (fun ca -> ca env)
            mI.Invoke(obj, args) :?> 'T

    | PropertyGet(obj, pI, args) ->
        let cobj = obj |> Option.map meaningUntyped
        let cargs = args |> Seq.map meaningUntyped |> Seq.toArray
        fun env ->
            let obj = match cobj with Some co -> co env | None -> null
            let args = cargs |> Array.map (fun ca -> ca env)
            pI.GetValue(obj, args) :?> 'T

    | PropertySet(obj, pI, args, value) ->
        let cobj = obj |> Option.map meaningUntyped
        let cargs = args |> Seq.map meaningUntyped |> Seq.toArray
        let cvalue = meaningUntyped value
        EQ(fun env ->
            let obj = match cobj with Some co -> co env | None -> null
            let args = cargs |> Array.map (fun ca -> ca env)
            let value = cvalue env
            pI.SetValue(obj, value, args))

    | FieldGet(obj, fI) ->
        let cobj = obj |> Option.map meaningUntyped
        fun env ->
            let obj = match cobj with Some co -> co env | None -> null
            fI.GetValue(obj) :?> 'T

    | FieldSet(obj, fI, value) ->
        let cobj = obj |> Option.map meaningUntyped
        let cvalue = meaningUntyped value
        EQ(fun env ->
            let obj = match cobj with Some co -> co env | None -> null
            let value = cvalue env
            fI.SetValue(obj, value))

    | _ -> failwithf "Unsupported expression %A" expr


let compile (e : Expr<'T>) : unit -> 'T  =
    let c = meaning e
    fun () -> c (Environment())

let run (e : Expr<'T>) : 'T = 
    compile e ()


//----------------------------
// Examples

run <@  match { contents = (Some 2) } with 
        | { contents = Some x } -> x + 1 
        | { contents = None } -> -1   @>

let deMorgan = run <@ fun x y -> not (x || y) = (not x && not y) @>

[for x in [false;true] do for y in [false;true] -> deMorgan x y] |> List.forall id

let factorial =
    run <@ 
            let rec factorial n = 
                if n = 0 then 1 
                else n * factorial (n - 1)

            factorial
        @>

[for i in 1 .. 10 -> factorial i]

let fib =
    run <@
            let rec fib n =
                match n with
                | 0 | 1 -> n
                | _ -> fib(n-2) + fib(n-1)

            fib
        @>


run <@ [for i in 1 .. 10 -> run <@ fib i @> ] @>

let even, odd =
    run <@
            let rec even n =
                if n = 0 then true
                else odd (n - 1)

            and odd n =
                if n = 0 then false
                else even (n - 1)

            even, odd
        @>


[for i in 1 .. 10 -> even i ]

//---------------------
// Perf

#r "../packages/Unquote/lib/net45/Unquote.dll"

let factorialExpr =
    <@ 
        let rec factorial n = 
            if n = 0 then 1 
            else n * factorial (n - 1)

        factorial
    @>

let rec baselineF n = if n = 0 then 1 else n * baselineF(n - 1)
let compiledF = run factorialExpr
let unquoteF = Swensen.Unquote.Operators.eval factorialExpr

#time "on"

// Real: 00:00:00.003, CPU: 00:00:00.015, GC gen0: 0, gen1: 0, gen2: 0
for i = 1 to 100000 do
    let _ = baselineF 10 in ()

// Real: 00:00:07.734, CPU: 00:00:07.671, GC gen0: 1874, gen1: 1, gen2: 0
for i = 1 to 100000 do
    let _ = unquoteF 10 in ()

// Real: 00:00:00.402, CPU: 00:00:00.390, GC gen0: 113, gen1: 1, gen2: 0
for i = 1 to 100000 do
    let _ = compiledF 10 in ()