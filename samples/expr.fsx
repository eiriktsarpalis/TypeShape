#r "../bin/TypeShape.dll"

open System
open System.Collections.Generic
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape
open TypeShape

type CompiledExpr<'T> = Environment -> 'T

and Environment private (index : Map<Var, obj ref>) =
    new () = new Environment(Map.empty)
    member __.NewVar(v : Var, value : obj) = new Environment(Map.add v (ref value) index)
    member __.GetVar(v : Var) = index.[v].Value
    member __.UpdateVar(v : Var, value : obj) = index.[v] := value


let rec meaning<'T> (expr : Expr<'T>) : CompiledExpr<'T> =
    let EQ(f : CompiledExpr<'a>) = unbox<CompiledExpr<'T>> f
    let cast (e : Expr) = Expr.Cast<_> e

    match expr with
    | Value(:? 'T as t, _) -> fun _ -> t
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
        let cleft = meaning<bool> (cast pred)
        EQ(not << cleft)

    | NewTuple exprs ->
        match shapeof<'T> with
        | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
            let cElem (s : IShapeWriteMember<'T>, e : Expr) =
                s.Accept { new IWriteMemberVisitor<'T, Environment -> 'T -> 'T> with
                    member __.Visit(s : ShapeWriteMember<'T, 'Elem>) =
                        let cElem = meaning<'Elem> (cast e)
                        fun env t -> let e = cElem env in s.Inject t e }

            let celems =
                Seq.zip shape.Elements exprs
                |> Seq.map cElem
                |> Seq.toArray

            fun env ->
                let mutable tuple = shape.CreateUninitialized()
                for ce in celems do tuple <- ce env tuple
                tuple

        | _ -> failwith "internal error"

    | TupleGet (tuple, index) ->
        match TypeShape.Create tuple.Type with
        | Shape.Tuple s ->
            s.Accept { new ITupleVisitor<CompiledExpr<'T>> with
                member __.Visit<'Tuple> (shape : ShapeTuple<'Tuple>) =
                    let es = shape.Elements.[index] :?> ShapeWriteMember<'Tuple, 'T>
                    let ctuple = meaning<'Tuple> (cast tuple)
                    fun env -> let tup = ctuple env in es.Project tup }

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

    | _ -> failwithf "Unsupported expression %A" expr


let compile (e : Expr<'T>) : unit -> 'T = 
    let c = meaning e
    fun () -> c (Environment())

let run (e : Expr<'T>) : 'T = 
    compile e ()


//----------------------------
// Examples

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

[for i in 1 .. 10 -> fib i]


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

#r "../bin/Unquote.dll"

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