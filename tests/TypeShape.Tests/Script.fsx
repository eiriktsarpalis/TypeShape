#r "../../bin/Debug/net40/TypeShape.dll"
open System
open TypeShape.Core


let foo () =
    let x = new obj()
    let y = 42
    <@ (x,y,2) @>

<@ fun x -> { x with contents = 42 } @>


<@ fun x y -> x + 1 = y @>