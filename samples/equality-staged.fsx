#r "../bin/Release/net40/TypeShape.dll"
#r "../bin/Release/net40/Unquote.dll"

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
                fun dt dt' ->
                    fcmp (shape.ProjectExpr dt) 
                         (shape.ProjectExpr dt') }

    match shapeof<'T> with
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
                                i <- i + 1
                            areEqual
                    @> )}

    | Shape.FSharpOption s ->
        s.Accept { new IFSharpOptionVisitor<CmpExpr<'T>> with
            member __.Visit<'t> () =
                let ec = stageCmp<'t> ()
                wrap(fun topt topt' ->
                    <@
                        match %topt, %topt' with
                        | None, None -> true
                        | None, _ | _, None -> false
                        | Some t, Some t' -> (% Expr.lam2 ec) t t'
                    @> )}

    | Shape.FSharpList s ->
        s.Accept { new IFSharpListVisitor<CmpExpr<'T>> with
            member __.Visit<'t> () =
                let ec = stageCmp<'t> ()
                wrap(fun ts ts' ->
                    <@
                        let rec aux ts ts' =
                            match ts, ts' with
                            | [], [] -> true
                            | t :: tl, t' :: tl' when (% Expr.lam2 ec) t t' -> aux tl tl'
                            | _ -> false

                        aux %ts %ts'
                    @> ) }

    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
        let elemCmps = shape.Elements |> Array.map stageMemberCmp
        fun t1 t2 ->
            <@
                let t1 = %t1
                let t2 = %t2
                (% Expr.lam2 (fun t1 t2 ->
                    elemCmps 
                    |> Array.map (fun ec -> ec t1 t2)
                    |> Expr.forall)) t1 t2 @>

    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
        let fldCmps = shape.Fields |> Array.map stageMemberCmp
        fun r1 r2 ->
            <@
                let r1 = %r1
                let r2 = %r2
                (% Expr.lam2 (fun r1 r2 ->
                    fldCmps
                    |> Array.map (fun ec -> ec r1 r2)
                    |> Expr.forall)) r1 r2 @>

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
        let stageUnionCaseCmp (case : ShapeFSharpUnionCase<'T>) =
            let fldCmps = case.Fields |> Array.map stageMemberCmp
            fun u1 u2 ->
                fldCmps
                |> Array.map (fun ec -> ec u1 u2)
                |> Expr.forall

        let unionCaseCmps = shape.UnionCases |> Array.map stageUnionCaseCmp

        fun u1 u2 ->
            let caseCmps = unionCaseCmps |> Array.map (fun cmp -> cmp u1 u2)
            <@
                let u1 = %u1
                let u2 = %u2
                let tag1 = (% Expr.lam shape.GetTagExpr) u1
                let tag2 = (% Expr.lam shape.GetTagExpr) u2
                if tag1 <> tag2 then false else
                (% Expr.lam (fun tag -> Expr.switch tag caseCmps)) tag1
            @>

    | _ -> failwithf "Unsupported shape %O" typeof<'T>

let mkComparerExpr<'T>() = stageCmp<'T>() |> Expr.lam2 |> Expr.cleanup
let mkComparer<'T> () = mkComparerExpr<'T>() |> eval
let decompileCmp<'T> () = mkComparerExpr<'T>() |> decompile

// examples

let cmp = mkComparer<int list * string option>()

cmp ([1 .. 100], Some "42") ([1 .. 100], Some "42")

decompileCmp<int * (int * int)>()
// fun t1 t2 -> 
//     t1.m_Item1 = t2.m_Item1 && 
//     let t1 = t1.m_Item2 
//     let t2 = t2.m_Item2 
//     t1.m_Item1 = t2.m_Item1 && t1.m_Item2 = t2.m_Item2
   
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
//fun t1 t2 -> 
//    let tag1 = t1.Tag 
//    let tag2 = t2.Tag 
//    if tag1 <> tag2 then false else 
//    tag1 = 0 || 
//    if tag1 = 1 then t1._foo = t2._foo 
//    else if tag1 = 2 then 
//        let r1 = t1.item 
//        let r2 = t2.item 
//        r1.A@ = r2.A@ && r1.B@ = r2.B@ 
//    else invalidOp "invalid tag"