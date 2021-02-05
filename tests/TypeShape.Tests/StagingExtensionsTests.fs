module TypeShape.Tests.StagingExtensions

open Xunit
open TypeShape.Core.StagingExtensions
open Swensen.Unquote


[<Fact>]
let ``'unlet' with option`` () =
    let v = Some 1

    let a = let x = <@ v @> in <@ if (%x).ToString() = "2" then 3 else (%x).Value @>
    let b = <@ let x = v in if x.ToString() = "2" then 3 else x.Value @>

    test <@ %a = %b @>
    test <@ a = Expr.unlet b @>

[<Fact>]
let ``'unlet' with value option`` () =
    let v = ValueSome 4

    let a = let x = <@ v @> in <@ if (%x).ToString() = "5" then 6 else (%x).Value @>
    let b = <@ let x = v in if x.ToString() = "5" then 6 else x.Value @>

    test <@ %a = %b @>
    test <@ Expr.unlet a = Expr.unlet b @>

[<Fact>]
let ``'unlambda' with splicing and direct application`` () =
    let v = Some 9
    let a = let x = <@ v @> in <@ (%x).Value @>
    let b = <@ v.Value @>

    let actual   = let f = <@ (=) @> in <@ (%f) %a %b @> |> Expr.unlambda 
    let expected = <@ %a = %b @>

    test <@ %expected = %actual @>
    test <@  expected = actual  @>

