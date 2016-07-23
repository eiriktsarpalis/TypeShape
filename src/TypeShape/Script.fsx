#r "../../bin/TypeShape.dll"
open TypeShape

type Foo = { A : int ; B : string }

let shape = shapeof<Foo> :?> ShapeFSharpRecord<Foo, int, string>
shape.Ctor(2, "42")
shape.Proj2 { A = 2 ; B = null}