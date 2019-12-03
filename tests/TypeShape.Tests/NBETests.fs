module TypeShape.Tests.NBE

open System
open FSharp.Quotations
open Swensen.Unquote
open Xunit
open TypeShape.Core.NBE

#nowarn "0020"

let shouldEqual (expected : Expr<'T>) (actual : Expr<'T>) = Assert.Equal(decompile expected, decompile actual)

[<Fact>]
let ``Constant expression`` () =
    nbe <@ fun () -> 1 + 1 @> |> shouldEqual <@ fun () -> 2 @>

[<Fact>]
let ``Simple pipe elimination`` () =
    let f x = x + 1
    nbe <@ fun x -> x |> f @> |> shouldEqual <@ fun x -> f x @>
    nbe <@ fun x -> f <| x @> |> shouldEqual <@ fun x -> f x @>

[<Fact>]
let ``Simple ignore elimination`` () =
    nbe <@ ignore (1 + 1) @> |> shouldEqual <@ () @>
    nbe <@ ignore (sprintf "hello") @> |> shouldEqual <@ sprintf "hello" ; () @>

[<Fact>]
let ``Let value elimination`` () =
    nbe <@ fun () -> let x = 1 in x @> |> shouldEqual <@ fun () -> 1 @>

[<Fact>]
let ``Unused Let value elimination`` () =
    nbe <@ fun () -> let f x = x + 2 in let y = f 2 in 1 @> |> shouldEqual <@ fun () -> 1 @>

[<Fact>]
let ``No mutable Let elimination`` () =
    nbe <@ fun () -> let f x = x + 2 in let mutable y = f 2 in 1 @> 
    |> shouldEqual <@ fun () -> let mutable y = 4 in 1 @>

[<Fact>]
let ``No side-effectful let elimination`` () =
    nbe <@ fun x -> let f = printfn "hi" in x + 2 @> 
    |> shouldEqual <@ fun x -> let f = printfn "hi" in x + 2 @> 

[<Fact>]
let ``Delayed side-effectful let elimination`` () =
    nbe <@ fun x -> let f () = printfn "hi" in x + 2 @> 
    |> shouldEqual <@ fun x -> x + 2 @> 

[<Fact>]
let ``Should not eliminate function if scope is escaped`` () =
    nbe <@ fun x -> let f x = x + 1 in f, f x, f 1 @> |> shouldEqual <@ fun x -> let f x = x + 1 in f, x + 1, 2 @>

[<Fact>]
let ``Simple branch elimination`` () =
    nbe <@
        let f x =
            if x > 0 then printfn "Positive"
            elif x = 0 then printfn "Zero"
            else printfn "Negative"

        f -1; f 0; f 1
    @>

    |> shouldEqual <@ printfn "Negative" ; printfn "Zero" ; printfn "Positive" @>

[<Fact>]
let ``Simple Tuple elimination`` () =
    nbe <@ fun x -> let z,_ = (x + 1, "baz") in z @> |> shouldEqual <@ fun x -> x + 1 @>

[<Fact>]
let ``Combined Tuple, function and binding elimination`` () = 
    nbe <@ fun x -> let y,f = 1, fun x y -> x + y in let g x = f x in (f x y, g x y) @> 
    |> shouldEqual <@ fun x -> (x + 1, x + 1) @>

[<Fact>]
let ``Simple record elimination`` () =
    nbe <@
        fun z ->
            let x = {| x = z + 1 |}
            (x.x, x.x)
    @> 
    
    |> shouldEqual <@ fun z -> (z + 1, z + 1) @>

[<Fact>]
let ``Simple record elimination 2`` () =
    nbe <@
        let x = {| x = 1 |}
        let f (x : {| x : int |}) = x.x
        (f x, f x)
    @> 
    
    |> shouldEqual <@ (1,1) @>

[<Fact>]
let ``Should not eliminate record if escaping scope`` () =
    nbe <@
        let x = {| x = 1 |}
        (x,x.x)
    @> 
    
    |> shouldEqual <@ let x = {| x = 1 |} in (x,1) @>

[<Fact>]
let ``Simple pattern match elimination`` () =
    nbe <@ 
        fun x -> 
            match Some x with 
            | Some x -> x + 1 
            | None -> -1 
    @> 

    |> shouldEqual <@ fun x -> x + 1 @>

[<Fact>]
let ``Parameterized pattern match`` () =
    nbe <@ match Some 1 with Some x -> x + 1 | None -> -1 @> |> shouldEqual <@ 2 @>

[<Fact>]
let ``Pattern match elimination in function call`` () =
    nbe <@
        let f x =
            match x with
            | Some i -> i > 0
            | None -> false

        f (Some 42) , f None
    @> 
    
    |> shouldEqual <@ (true, false) @>

[<Fact>]
let ``Simple beta reduction`` () =
    nbe <@ fun z -> (fun x -> x + 1) z @>

    |> shouldEqual <@ fun z -> z + 1 @>

[<Fact>]
let ``Beta reduction should respect evaluation semantics`` () =
    nbe <@ fun x -> (fun x -> x,x) (printfn "hello" ; x + 1) @>

    |> shouldEqual <@ fun x -> let x = printfn "hello" ; x + 1 in x,x @>

[<Fact>]
let ``Beta reduction should not optimize away side-effects`` () =
    nbe <@ let f (x : int) = () in f (printfn "hi"; 42) @> 
    
    |> shouldEqual <@ let x = printfn "hi"; 42 in () @>


[<Fact>]
let ``Closure variable capturing should be preserved`` () =
    nbe <@ let x = (1,2) in let g () = x in g @>

    |> shouldEqual <@ let x = (1,2) in fun () -> x @>

[<Fact>]
let ``Let bindings should be eliminated in expressions that do not reference them`` () =
    nbe
        <@
            let g c =
                let f () = Console.Write "x"
                if c then
                    (f,f)
                else
                    (ignore, ignore)

            (g true , g false)
        @>

    |> shouldEqual <@ (let f () = Console.Write "x" in (f,f)), ((fun () -> ()), (fun () -> ())) @>

[<Fact>]
let ``Simple eta reduction`` () =
    nbe <@ fun z -> let f x = x + z in fun x -> f x @>

    |> shouldEqual <@ fun z x -> x + z @>

[<Fact>]
let ``Recursive function support`` () =
    nbe <@ let rec f x = if x = 0 then (2 + (-1)) else x * f(x - 1) in f @>
    |> shouldEqual <@ let rec f x = if x = 0 then 1 else x * f(x - 1) in f @>

[<Fact>]
let ``Imperative code support`` () =
    nbe <@
        fun x ->
            let mutable isPrime = true
            let mutable i = 1 + 1
            while isPrime && i < x / (1 + 1) do
                if x % i = 0 then isPrime <- false
                else i <- i + (2 + (-1))

            if isPrime then failwith "omg"
    @> 
    
    |> shouldEqual <@ fun x ->
        let mutable isPrime = true
        let mutable i = 2
        while isPrime && i < x / 2 do
            if x % i = 0 then isPrime <- false
            else i <- i + 1

        if isPrime then failwith "omg"
    @> 

type Stream<'T> = ('T -> unit) -> unit

[<Fact>]
let ``Stream inlining`` () =
    nbe <@
        let ofArray (xs : int []) =
            fun k -> for x in xs do k x

        let map : (int -> int) -> Stream<int> -> Stream<int> =
            fun f ts k -> ts (fun t -> k (f t))

        let filter : (int -> bool) -> Stream<int> -> Stream<int> =
            fun f ts k -> ts (fun t -> if (f t) then k t)

        let iter : (int -> unit) -> Stream<int> -> unit =
            fun f ts -> ts f

        fun xs ->
            ofArray xs
            |> filter (fun i -> i > 0)
            |> filter (fun i -> i % 2 = 0)
            |> map (fun i -> i * i)
            |> map (fun i -> i + i)
            |> iter (fun i -> printfn "%d" i)
    @> 
    
    |> shouldEqual <@ fun (xs : int []) ->
        for x in xs do
            if x > 0 then
                if x % 2 = 0 then
                    let t = x * x
                    let i = t + t
                    printfn "%d" i
    @>