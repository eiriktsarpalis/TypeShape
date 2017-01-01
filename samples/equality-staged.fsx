#r "../bin/TypeShape.dll"
#r "../bin/Unquote.dll"

open TypeShape
open TypeShape_StagingExtensions
open Swensen.Unquote
open FSharp.Quotations

type CmpExpr<'T> = Expr<'T> -> Expr<'T> -> Expr<bool>

let rec stageCmp<'T> () : CmpExpr<'T> =
    let wrap (cmp : CmpExpr<'a>) = unbox<CmpExpr<'T>> cmp

    let stageMemberCmp (shape : IShapeMember<'DeclaringType>) =
        shape.Accept { new IMemberVisitor<'DeclaringType, CmpExpr<'DeclaringType>> with
            member __.Visit (shape : ShapeMember<'DeclaringType, 'FieldType>) =
                let fcmp = stageCmp<'FieldType>()
                fun (dt : Expr<'DeclaringType>) dt' -> 
                    fcmp (shape.ProjectExpr dt) (shape.ProjectExpr dt')
        }

    match TypeShape.Create<'T> () with
    | Shape.Unit -> wrap(fun (_: Expr<unit>) _ -> <@ true @>)
    | Shape.Bool -> wrap(fun (b: Expr<bool>) b' -> <@ %b = %b' @>)
    | Shape.Int32 -> wrap(fun (n: Expr<int>) n' -> <@ %n = %n' @>)
    | Shape.Double -> wrap(fun (d: Expr<double>) d' -> <@ %d = %d' @>)
    | Shape.String -> wrap(fun (s: Expr<string>) s' -> <@ %s = %s' @>)
    | Shape.Array s when s.Rank = 1 ->
        s.Accept { new IArrayVisitor<CmpExpr<'T>> with
            member __.Visit<'t> _ =
                let ec = stageCmp<'t>()
                wrap(fun (ts : Expr<'t []>) ts' ->
                    <@
                        match %ts, %ts' with
                        | null, null -> true
                        | null, _ | _, null -> false
                        | ts, ts' when ts.Length <> ts'.Length -> false
                        | ts, ts' ->
                            let mutable i = 0
                            let mutable areEqual = true
                            while areEqual && i < ts.Length do
                                areEqual <- (% Expr.lam2 ec) ts.[i] ts'.[i]
                            areEqual
                    @>)
        }

    | Shape.FSharpOption s ->
        s.Accept { new IFSharpOptionVisitor<CmpExpr<'T>> with
            member __.Visit<'t> () =
                let ec = stageCmp<'t> ()
                wrap(fun (ts : Expr<'t option>) ts' ->
                    <@
                        match %ts, %ts' with
                        | None, None -> true
                        | None, _ | _, None -> false
                        | Some t, Some t' -> (% Expr.lam2 ec) t t'
                    @>
                )

        }

    | Shape.FSharpList s ->
        s.Accept { new IFSharpListVisitor<CmpExpr<'T>> with
            member __.Visit<'t> () =
                let ec = stageCmp<'t> ()
                wrap(fun (ts: Expr<'t list>) ts' ->
                    <@
                        let rec aux ts ts' =
                            match ts, ts' with
                            | [], [] -> true
                            | t :: tl, t' :: tl' when (% Expr.lam2 ec) t t' -> aux tl tl'
                            | _ -> false

                        aux %ts %ts'
                    @>)
        }

    | Shape.Tuple s ->
        s.Accept { new ITupleVisitor<CmpExpr<'T>> with
            member __.Visit (shape : ShapeTuple<'Tuple>) =
                let elemCmps = shape.Elements |> Array.map stageMemberCmp
                wrap (fun (t1 : Expr<'Tuple>) t2 ->
                    elemCmps 
                    |> Array.map (fun ec -> ec t1 t2)
                    |> Expr.forall)
        }

    | Shape.FSharpRecord s ->
        s.Accept { new IFSharpRecordVisitor<CmpExpr<'T>> with
            member __.Visit (shape : ShapeFSharpRecord<'Record>) =
                let fldCmps = shape.Fields |> Array.map stageMemberCmp
                wrap (fun (r1 : Expr<'Record>) r2 ->
                    fldCmps
                    |> Array.map (fun ec -> ec r1 r2)
                    |> Expr.forall) 
        }

    | Shape.FSharpUnion s ->
        s.Accept { new IFSharpUnionVisitor<CmpExpr<'T>> with
            member __.Visit (shape : ShapeFSharpUnion<'Union>) =
                let stageUnionCaseCmp (case : ShapeFSharpUnionCase<'Union>) =
                    let fldCmps = case.Fields |> Array.map stageMemberCmp
                    fun (u1 : Expr<'Union>) u2 ->
                        fldCmps
                        |> Array.map (fun ec -> ec u1 u2)
                        |> Expr.forall

                let unionCaseCmps = shape.UnionCases |> Array.map stageUnionCaseCmp

                wrap(fun (u1 : Expr<'Union>) u2 ->
                    let caseCmps = unionCaseCmps |> Array.map (fun cmp -> cmp u1 u2)
                    <@
                        let t1 = (% shape.GetTagExpr u1)
                        let t2 = (% shape.GetTagExpr u2)
                        if t1 <> t2 then false else
                        (% Expr.lam (fun tag -> Expr.switch tag caseCmps)) t1
                    @>)
        }

    | _ -> failwithf "Unsupported shape %O" typeof<'T>

let mkComparer<'T> () =
    let expr = stageCmp<'T>() |> Expr.lam2 |> Expr.unlambda
    eval expr

let decompileCmp<'T> () =
    stageCmp<'T>() |> Expr.lam2 |> Expr.unlambda |> decompile

// examples

let cmp = mkComparer<int list * string option>()

decompileCmp<string * int * int * string>()

cmp ([1 .. 100], Some "42") ([1 .. 100], Some "42")
   
type Foo = { A : int ; B : string }

type Bar =
    | UA
    | UB of foo:string
    | UC of Foo

let cmp' = mkComparer<Bar>()

cmp' UA UA
cmp' (UC { A = 12 ; B = "test" })
     (UC { A = 12 ; B = "test2" })

decompileCmp<Bar>()