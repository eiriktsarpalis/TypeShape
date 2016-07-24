module TypeShape.Tests

open System
open System.Reflection

open Xunit
open Swensen.Unquote.Assertions
open FsCheck

open TypeShape

let testPrim<'T>() = test <@ shapeof<'T>.GetType() = typeof<TypeShape<'T>> @>

[<Fact>]
let ``Shape primitive`` () =
    testPrim<bool>() ; testPrim<byte>() ; testPrim<sbyte>()
    testPrim<int16>() ; testPrim<int32>() ; testPrim<int64>()
    testPrim<uint16>() ; testPrim<uint32>() ; testPrim<uint64>()

[<Fact>]
let ``Shape BCL primitives`` () =
    testPrim<DateTime>() ; testPrim<DateTimeOffset>()

[<Fact>]
let ``Shape Binding Flags`` () =
    test <@ match shapeof<BindingFlags> with ShapeEnum _ -> true | _ -> false @>