module TypeShape.Tests

open System
open System.Collections.Generic
open System.Reflection

open Xunit
open Swensen.Unquote.Assertions
open FsCheck

open TypeShape

let check<'T>(prop : 'T -> bool) = Check.QuickThrowOnFailure prop

let testPrim<'T>() = 
    let shape = shapeof<'T>
    test <@ shape.GetType() = typeof<TypeShape<'T>> @>
    let accepter = { new ITypeShapeVisitor<bool> with member __.Visit<'a>() = typeof<'T> = typeof<'a> }
    test <@ shape.Accept accepter @>

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
    let accepter = 
        { new IEnumVisitor<bool> with 
            member __.Visit<'T, 'U when 'T : enum<'U>>() = 
                typeof<'T> = typeof<BindingFlags> && typeof<'U> = typeof<int> }
    test <@ match shapeof<BindingFlags> with Shape.Enum e -> e.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape Nullable`` () =
    let accepter = 
        { new INullableVisitor<bool> with 
            member __.Visit<'T when 'T : struct and 'T : (new : unit -> 'T) and 'T :> ValueType>() = 
                typeof<'T> = typeof<int> }
    test <@ match shapeof<Nullable<int>> with Shape.Nullable e -> e.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape Tuple`1`` () =
    let accepter = 
        { new ITuple1Visitor<bool> with 
            member __.Visit<'T>() = typeof<'T> = typeof<int> }
    test <@ match shapeof<Tuple<int>> with Shape.Tuple1 e -> e.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape Tuple`2`` () =
    let accepter = 
        { new ITuple2Visitor<bool> with 
            member __.Visit<'T1, 'T2>() = typeof<'T1> = typeof<int> && typeof<'T2> = typeof<string> }
    test <@ match shapeof<int * string> with Shape.Tuple2 e -> e.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape Tuple`3`` () =
    let accepter = 
        { new ITuple3Visitor<bool> with 
            member __.Visit<'T1, 'T2, 'T3>() = 
                typeof<'T1> = typeof<int> && 
                typeof<'T2> = typeof<string> && 
                typeof<'T3> = typeof<bool> }
    test <@ match shapeof<int * string * bool> with Shape.Tuple3 e -> e.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape Tuple`4`` () =
    let accepter = 
        { new ITuple4Visitor<bool> with 
            member __.Visit<'T1, 'T2, 'T3, 'T4>() = 
                typeof<'T1> = typeof<int> && 
                typeof<'T2> = typeof<string> && 
                typeof<'T3> = typeof<bool> &&
                typeof<'T4> = typeof<byte> }
    test <@ match shapeof<int * string * bool * byte> with Shape.Tuple4 e -> e.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape Tuple`5`` () =
    let accepter = 
        { new ITuple5Visitor<bool> with 
            member __.Visit<'T1, 'T2, 'T3, 'T4, 'T5>() = 
                typeof<'T1> = typeof<int> && 
                typeof<'T2> = typeof<string> && 
                typeof<'T3> = typeof<bool> &&
                typeof<'T4> = typeof<byte> &&
                typeof<'T5> = typeof<sbyte> }
    test <@ match shapeof<int * string * bool * byte * sbyte> with Shape.Tuple5 e -> e.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape Tuple`6`` () =
    let accepter = 
        { new ITuple6Visitor<bool> with 
            member __.Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6>() = 
                typeof<'T1> = typeof<int> && 
                typeof<'T2> = typeof<string> && 
                typeof<'T3> = typeof<bool> &&
                typeof<'T4> = typeof<byte> &&
                typeof<'T5> = typeof<sbyte> &&
                typeof<'T6> = typeof<int16> }
    test <@ match shapeof<int * string * bool * byte * sbyte * int16> with Shape.Tuple6 e -> e.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape Tuple`7`` () =
    let accepter = 
        { new ITuple7Visitor<bool> with 
            member __.Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7>() = 
                typeof<'T1> = typeof<int> && 
                typeof<'T2> = typeof<string> && 
                typeof<'T3> = typeof<bool> &&
                typeof<'T4> = typeof<byte> &&
                typeof<'T5> = typeof<sbyte> &&
                typeof<'T6> = typeof<int16> && 
                typeof<'T7> = typeof<int64>}
    test <@ match shapeof<int * string * bool * byte * sbyte * int16 * int64> with Shape.Tuple7 e -> e.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape Tuple`8`` () =
    let accepter = 
        { new ITuple8Visitor<bool> with 
            member __.Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'TRest>() = 
                typeof<'T1> = typeof<int> && 
                typeof<'T2> = typeof<string> && 
                typeof<'T3> = typeof<bool> &&
                typeof<'T4> = typeof<byte> &&
                typeof<'T5> = typeof<sbyte> &&
                typeof<'T6> = typeof<int16> && 
                typeof<'T7> = typeof<int64> &&
                typeof<'TRest> = typeof<Tuple<int>> }
    test <@ match shapeof<int * string * bool * byte * sbyte * int16 * int64 * int> with Shape.Tuple8 e -> e.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape FSharpFunc`` () =
    let accepter =
        { new IFSharpFuncVisitor<bool> with
            member __.Visit<'D,'C>() = typeof<'D> = typeof<int> && typeof<'C> = typeof<string>}
    test <@ match shapeof<int -> string> with Shape.FSharpFunc s -> s.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape Exception`` () =
    let accepter =
        { new IExceptionVisitor<bool> with
            member __.Visit<'exn when 'exn :> exn>() = typeof<'exn> = typeof<System.IO.FileNotFoundException> }
    test <@ match shapeof<System.IO.FileNotFoundException> with Shape.Exception s -> s.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape Delegate`` () =
    let accepter =
        { new IDelegateVisitor<bool> with
            member __.Visit<'Delegate when 'Delegate :> Delegate>() = typeof<'Delegate> = typeof<Predicate<string>> }
    test <@ match shapeof<Predicate<string>> with Shape.Delegate s -> s.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape Enumerable`` () =
    let accepter t =
        { new IEnumerableVisitor<bool> with
            member __.Visit<'T>() = typeof<'T> = t }
    test <@ match shapeof<int []> with Shape.Enumerable s -> s.Accept (accepter typeof<int>) | _ -> false @>
    test <@ match shapeof<int list> with Shape.Enumerable s -> s.Accept (accepter typeof<int>) | _ -> false @>
    test <@ match shapeof<seq<int>> with Shape.Enumerable s -> s.Accept (accepter typeof<int>) | _ -> false @>
    test <@ match shapeof<ResizeArray<int>> with Shape.Enumerable s -> s.Accept (accepter typeof<int>) | _ -> false @>
    test <@ match shapeof<HashSet<int>> with Shape.Enumerable s -> s.Accept (accepter typeof<int>) | _ -> false @>
    test <@ match shapeof<Dictionary<int, string>> with Shape.Enumerable s -> s.Accept (accepter typeof<KeyValuePair<int, string>>) | _ -> false @>
    test <@ match shapeof<Set<int>> with Shape.Enumerable s -> s.Accept (accepter typeof<int>) | _ -> false @>
    test <@ match shapeof<Map<int, string>> with Shape.Enumerable s -> s.Accept (accepter typeof<KeyValuePair<int, string>>) | _ -> false @>

[<Fact>]
let ``Shape Collection`` () =
    let accepter t =
        { new ICollectionVisitor<bool> with
            member __.Visit<'T>() = typeof<'T> = t }
    test <@ match shapeof<int []> with Shape.Collection s -> s.Accept (accepter typeof<int>) | _ -> false @>
    test <@ match shapeof<int list> with Shape.Collection s -> s.Accept (accepter typeof<int>) | _ -> false @>
    test <@ match shapeof<seq<int>> with Shape.Collection s -> false | _ -> true @>
    test <@ match shapeof<ResizeArray<int>> with Shape.Collection s -> s.Accept (accepter typeof<int>) | _ -> false @>
    test <@ match shapeof<HashSet<int>> with Shape.Collection s -> s.Accept (accepter typeof<int>) | _ -> false @>
    test <@ match shapeof<Dictionary<int, string>> with Shape.Collection s -> s.Accept (accepter typeof<KeyValuePair<int, string>>) | _ -> false @>
    test <@ match shapeof<Set<int>> with Shape.Collection s -> s.Accept (accepter typeof<int>) | _ -> false @>
    test <@ match shapeof<Map<int, string>> with Shape.Collection s -> s.Accept (accepter typeof<KeyValuePair<int, string>>) | _ -> false @>

[<Fact>]
let ``Shape KeyValuePair`` () =
    let accepter = 
        { new IKeyValuePairVisitor<bool> with
            member __.Visit<'K,'V>() = typeof<'K> = typeof<int> && typeof<'V> = typeof<string> }

    test <@ match shapeof<KeyValuePair<int,string>> with Shape.KeyValuePair s -> s.Accept accepter | _ -> false @>


[<Fact>]
let ``Shape Array`` () =
    let accepter = 
        { new IArrayVisitor<bool> with
            member __.Visit<'T>() = typeof<'T> = typeof<int> }

    test <@ match shapeof<int []> with Shape.Array s -> s.Accept accepter | _ -> false @>    

[<Fact>]
let ``Shape Array 2D`` () =
    let accepter = 
        { new IArray2DVisitor<bool> with
            member __.Visit<'T>() = typeof<'T> = typeof<int> }

    test <@ match shapeof<int [,]> with Shape.Array2D s -> s.Accept accepter | _ -> false @>    

[<Fact>]
let ``Shape Array 3D`` () =
    let accepter = 
        { new IArray3DVisitor<bool> with
            member __.Visit<'T>() = typeof<'T> = typeof<int> }

    test <@ match shapeof<int [,,]> with Shape.Array3D s -> s.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape Array 4D`` () =
    let accepter = 
        { new IArray4DVisitor<bool> with
            member __.Visit<'T>() = typeof<'T> = typeof<int> }

    test <@ match shapeof<int [,,,]> with Shape.Array4D s -> s.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape ResizeArray`` () =
    let accepter = 
        { new IResizeArrayVisitor<bool> with
            member __.Visit<'T>() = typeof<'T> = typeof<int> }

    test <@ match shapeof<ResizeArray<int>> with Shape.ResizeArray s -> s.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape Dictionary`` () =
    let accepter = 
        { new IDictionaryVisitor<bool> with
            member __.Visit<'K, 'V when 'K : equality>() = typeof<'K> = typeof<int> && typeof<'V> = typeof<string> }

    test <@ match shapeof<Dictionary<int, string>> with Shape.Dictionary s -> s.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape F# Set`` () =
    let accepter = 
        { new IFSharpSetVisitor<bool> with
            member __.Visit<'T when 'T : comparison>() = typeof<'T> = typeof<string> }

    test <@ match shapeof<Set<string>> with Shape.FSharpSet s -> s.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape F# Map`` () =
    let accepter = 
        { new IFSharpMapVisitor<bool> with
            member __.Visit<'K, 'V when 'K : comparison>() = typeof<'K> = typeof<string> && typeof<'V> = typeof<int> }

    test <@ match shapeof<Map<string, int>> with Shape.FSharpMap s -> s.Accept accepter | _ -> false @>

type Record1 = { A1 : int }

[<Fact>]
let ``Shape Record 1`` () =
    test <@ match shapeof<Record1> with Shape.FSharpRecord1 _ -> true | _ -> false @>
    let shape = shapeof<Record1> |> unbox<IShapeFSharpRecord<Record1, int>>
    test <@ shape.Properties.Length = 1 @>
    test <@ shape.IsFSharpRef = false @>
    check(fun (r:Record1) -> shape.Construct(shape.Project1 r) = r)
    check(fun (a1:int) -> shape.Project1(shape.Construct a1) = a1)

type Record2 = { A1 : int ; A2 : string }

[<Fact>]
let ``Shape Record 2`` () =
    test <@ match shapeof<Record2> with Shape.FSharpRecord2 _ -> true | _ -> false @>
    let shape = shapeof<Record2> |> unbox<IShapeFSharpRecord<Record2, int, string>>
    test <@ shape.Properties.Length = 2 @>
    test <@ shape.IsFSharpRef = false @>
    check(fun (r:Record2) -> shape.Construct(shape.Project1 r, shape.Project2 r) = r)
    check(fun (a1:int, a2:string) -> 
        let r = shape.Construct(a1, a2)
        shape.Project1 r = a1 && shape.Project2 r = a2)

type Record3 = { A1 : int ; A2 : string ; A3 : bool }

[<Fact>]
let ``Shape Record 3`` () =
    test <@ match shapeof<Record3> with Shape.FSharpRecord3 _ -> true | _ -> false @>
    let shape = shapeof<Record3> |> unbox<IShapeFSharpRecord<Record3, int, string, bool>>
    test <@ shape.Properties.Length = 3 @>
    test <@ shape.IsFSharpRef = false @>
    check(fun (r:Record3) -> shape.Construct(shape.Project1 r, shape.Project2 r, shape.Project3 r) = r)
    check(fun (a1:int, a2:string, a3:bool) -> 
        let r = shape.Construct(a1, a2, a3)
        shape.Project1 r = a1 && shape.Project2 r = a2 &&
        shape.Project3 r = a3)

type Record4 = { A1 : int ; A2 : string ; A3 : bool ; A4 : byte }

[<Fact>]
let ``Shape Record 4`` () =
    test <@ match shapeof<Record4> with Shape.FSharpRecord4 _ -> true | _ -> false @>
    let shape = shapeof<Record4> |> unbox<IShapeFSharpRecord<Record4, int, string, bool, byte>>
    test <@ shape.Properties.Length = 4 @>
    test <@ shape.IsFSharpRef = false @>
    check(fun (r:Record4) -> shape.Construct(shape.Project1 r, shape.Project2 r, shape.Project3 r, shape.Project4 r) = r)
    check(fun (a1:int, a2:string, a3:bool, a4:byte) -> 
        let r = shape.Construct(a1, a2, a3, a4)
        shape.Project1 r = a1 && shape.Project2 r = a2 &&
        shape.Project3 r = a3 && shape.Project4 r = a4)

type Record5 = { A1 : int ; A2 : string ; A3 : bool ; A4 : byte ; A5 : byte[] }

[<Fact>]
let ``Shape Record 5`` () =
    test <@ match shapeof<Record5> with Shape.FSharpRecord5 _ -> true | _ -> false @>
    let shape = shapeof<Record5> |> unbox<IShapeFSharpRecord<Record5, int, string, bool, byte, byte[]>>
    test <@ shape.Properties.Length = 5 @>
    test <@ shape.IsFSharpRef = false @>
    check(fun (r:Record5) -> shape.Construct(shape.Project1 r, shape.Project2 r, shape.Project3 r, shape.Project4 r, shape.Project5 r) = r)
    check(fun (a1:int, a2:string, a3:bool, a4:byte, a5:byte[]) -> 
        let r = shape.Construct(a1, a2, a3, a4, a5)
        shape.Project1 r = a1 && shape.Project2 r = a2 &&
        shape.Project3 r = a3 && shape.Project4 r = a4 &&
        shape.Project5 r = a5)


type Record6 = { A1 : int ; A2 : string ; A3 : bool ; A4 : byte ; A5 : byte[] ; A6 : decimal }

[<Fact>]
let ``Shape Record 6`` () =
    test <@ match shapeof<Record6> with Shape.FSharpRecord6 _ -> true | _ -> false @>
    let shape = shapeof<Record6> |> unbox<IShapeFSharpRecord<Record6, int, string, bool, byte, byte[], decimal>>
    test <@ shape.Properties.Length = 6 @>
    test <@ shape.IsFSharpRef = false @>
    check(fun (r:Record6) -> shape.Construct(shape.Project1 r, shape.Project2 r, shape.Project3 r, shape.Project4 r, shape.Project5 r, shape.Project6 r) = r)
    check(fun (a1:int, a2:string, a3:bool, a4:byte, a5:byte[], a6:decimal) -> 
        let r = shape.Construct(a1, a2, a3, a4, a5, a6)
        shape.Project1 r = a1 && shape.Project2 r = a2 &&
        shape.Project3 r = a3 && shape.Project4 r = a4 &&
        shape.Project5 r = a5 && shape.Project6 r = a6)

type Record7 = { A1 : int ; A2 : string ; A3 : bool ; A4 : byte ; A5 : byte[] ; A6 : decimal ; A7 : int16 }

[<Fact>]
let ``Shape Record 7`` () =
    test <@ match shapeof<Record7> with Shape.FSharpRecord7 _ -> true | _ -> false @>
    let shape = shapeof<Record7> |> unbox<IShapeFSharpRecord<Record7, int, string, bool, byte, byte[], decimal, int16>>
    test <@ shape.Properties.Length = 7 @>
    test <@ shape.IsFSharpRef = false @>
    check(fun (r:Record7) -> shape.Construct(shape.Project1 r, shape.Project2 r, shape.Project3 r, shape.Project4 r, shape.Project5 r, shape.Project6 r, shape.Project7 r) = r)
    check(fun (a1:int, a2:string, a3:bool, a4:byte, a5:byte[], a6:decimal, a7:int16) -> 
        let r = shape.Construct(a1, a2, a3, a4, a5, a6, a7)
        shape.Project1 r = a1 && shape.Project2 r = a2 &&
        shape.Project3 r = a3 && shape.Project4 r = a4 &&
        shape.Project5 r = a5 && shape.Project6 r = a6 &&
        shape.Project7 r = a7)

[<Fact>]
let ``Shape F# ref`` () =
    test <@ match shapeof<int ref> with Shape.FSharpRecord1 r -> r.IsFSharpRef | _ -> false @>
    let accepter = { new IFSharpRefVisitor<bool> with member __.Visit<'T>() = typeof<'T> = typeof<int> }
    test <@ match shapeof<int ref> with Shape.FSharpRef s -> s.Accept accepter | _ -> false @>
    let shape = shapeof<int ref> |> unbox<IShapeFSharpRecord<int ref, int>>
    test <@ shape.Project1(ref 42) = 42 @>
    test <@ shape.Construct(42).Value = 42 @>


type Union1 = C1U1 of int * string

[<Fact>]
let ``Shape Union 1`` () =
    test <@ match shapeof<Union1> with Shape.FSharpUnion1 u -> not u.IsFSharpChoice | _ -> false @>
    let shape = shapeof<Union1> |> unbox<IShapeFSharpUnion<Union1, int * string>>
    test <@ not shape.IsFSharpChoice @>
    test <@ not shape.IsFSharpList @>
    test <@ not shape.IsFSharpOption @>
    check(fun (u:Union1) -> shape.Construct1(shape.Project u) = u )

type Union2 = 
    | C1U2 of int * string
    | C2U2

[<Fact>]
let ``Shape Union 2`` () =
    test <@ match shapeof<Union2> with Shape.FSharpUnion2 u -> not u.IsFSharpChoice | _ -> false @>
    let shape = shapeof<Union2> |> unbox<IShapeFSharpUnion<Union2, int * string, unit>>
    test <@ not shape.IsFSharpChoice @>
    test <@ not shape.IsFSharpList @>
    test <@ not shape.IsFSharpOption @>
    check(fun (u:Union2) -> 
        match shape.Project u with
        | Choice1Of2 t -> shape.Construct1 t = u
        | Choice2Of2 t -> shape.Construct2 t = u)

type Union3 = 
    | C1U3 of int * string
    | C2U3
    | C3U3 of int

[<Fact>]
let ``Shape Union 3`` () =
    test <@ match shapeof<Union3> with Shape.FSharpUnion3 u -> not u.IsFSharpChoice | _ -> false @>
    let shape = shapeof<Union3> |> unbox<IShapeFSharpUnion<Union3, int * string, unit, int>>
    test <@ not shape.IsFSharpChoice @>
    test <@ not shape.IsFSharpList @>
    test <@ not shape.IsFSharpOption @>
    check(fun (u:Union3) -> 
        match shape.Project u with
        | Choice1Of3 t -> shape.Construct1 t = u
        | Choice2Of3 t -> shape.Construct2 t = u
        | Choice3Of3 t -> shape.Construct3 t = u)

type Union4 = 
    | C1U4 of int * string
    | C2U4
    | C3U4 of int
    | C4U4 of unit

[<Fact>]
let ``Shape Union 4`` () =
    test <@ match shapeof<Union4> with Shape.FSharpUnion4 u -> not u.IsFSharpChoice | _ -> false @>
    let shape = shapeof<Union4> |> unbox<IShapeFSharpUnion<Union4, int * string, unit, int, unit>>
    test <@ not shape.IsFSharpChoice @>
    test <@ not shape.IsFSharpList @>
    test <@ not shape.IsFSharpOption @>
    check(fun (u:Union4) -> 
        match shape.Project u with
        | Choice1Of4 t -> shape.Construct1 t = u
        | Choice2Of4 t -> shape.Construct2 t = u
        | Choice3Of4 t -> shape.Construct3 t = u
        | Choice4Of4 t -> shape.Construct4 t = u)

type Union5 = 
    | C1U5 of int * string
    | C2U5
    | C3U5 of int
    | C4U5 of unit
    | C5U5 of int * bool * byte[]

[<Fact>]
let ``Shape Union 5`` () =
    test <@ match shapeof<Union5> with Shape.FSharpUnion5 u -> not u.IsFSharpChoice | _ -> false @>
    let shape = shapeof<Union5> |> unbox<IShapeFSharpUnion<Union5, int * string, unit, int, unit, int * bool * byte[]>>
    test <@ not shape.IsFSharpChoice @>
    test <@ not shape.IsFSharpList @>
    test <@ not shape.IsFSharpOption @>
    check(fun (u:Union5) -> 
        match shape.Project u with
        | Choice1Of5 t -> shape.Construct1 t = u
        | Choice2Of5 t -> shape.Construct2 t = u
        | Choice3Of5 t -> shape.Construct3 t = u
        | Choice4Of5 t -> shape.Construct4 t = u
        | Choice5Of5 t -> shape.Construct5 t = u)

type Union6 = 
    | C1U6 of int * string
    | C2U6
    | C3U6 of int
    | C4U6 of unit
    | C5U6 of int * bool * byte[]
    | C6U6 of string

[<Fact>]
let ``Shape Union 6`` () =
    test <@ match shapeof<Union6> with Shape.FSharpUnion6 u -> not u.IsFSharpChoice | _ -> false @>
    let shape = shapeof<Union6> |> unbox<IShapeFSharpUnion<Union6, int * string, unit, int, unit, int * bool * byte[], string>>
    test <@ not shape.IsFSharpChoice @>
    test <@ not shape.IsFSharpList @>
    test <@ not shape.IsFSharpOption @>
    check(fun (u:Union6) -> 
        match shape.Project u with
        | Choice1Of6 t -> shape.Construct1 t = u
        | Choice2Of6 t -> shape.Construct2 t = u
        | Choice3Of6 t -> shape.Construct3 t = u
        | Choice4Of6 t -> shape.Construct4 t = u
        | Choice5Of6 t -> shape.Construct5 t = u
        | Choice6Of6 t -> shape.Construct6 t = u)

type Union7 = 
    | C1U7 of int * string
    | C2U7
    | C3U7 of int
    | C4U7 of unit
    | C5U7 of int * bool * byte[]
    | C6U7 of string
    | C7U7

[<Fact>]
let ``Shape Union 7`` () =
    test <@ match shapeof<Union7> with Shape.FSharpUnion7 u -> not u.IsFSharpChoice | _ -> false @>
    let shape = shapeof<Union7> |> unbox<IShapeFSharpUnion<Union7, int * string, unit, int, unit, int * bool * byte[], string, unit>>
    test <@ not shape.IsFSharpChoice @>
    test <@ not shape.IsFSharpList @>
    test <@ not shape.IsFSharpOption @>
    check(fun (u:Union7) -> 
        match shape.Project u with
        | Choice1Of7 t -> shape.Construct1 t = u
        | Choice2Of7 t -> shape.Construct2 t = u
        | Choice3Of7 t -> shape.Construct3 t = u
        | Choice4Of7 t -> shape.Construct4 t = u
        | Choice5Of7 t -> shape.Construct5 t = u
        | Choice6Of7 t -> shape.Construct6 t = u
        | Choice7Of7 t -> shape.Construct7 t = u)


[<Fact>]
let ``Shape F# Option`` () =
    test <@ match shapeof<int option> with Shape.FSharpUnion u -> u.IsFSharpOption | _ -> false @>
    test <@ match shapeof<int option> with Shape.FSharpOption _ -> true | _ -> false @>
    let shape = shapeof<int option> |> unbox<IShapeFSharpUnion<int option, unit, int>>
    test <@ shape.Project None = Choice1Of2 () @>
    test <@ shape.Project (Some 42) = Choice2Of2 42 @>
    test <@ shape.Construct1 () = None @>
    test <@ shape.Construct2 42 = Some 42 @>

[<Fact>]
let ``Shape F# list`` () =
    test <@ match shapeof<int list> with Shape.FSharpUnion u -> u.IsFSharpList | _ -> false @>
    test <@ match shapeof<int list> with Shape.FSharpList _ -> true | _ -> false @>
    let shape = shapeof<int list> |> unbox<IShapeFSharpUnion<int list, unit, int * int list>>
    test <@ shape.Project [] = Choice1Of2 () @>
    test <@ shape.Project [1;2] = Choice2Of2(1,[2]) @>
    test <@ shape.Construct1 () = [] @>
    test <@ shape.Construct2 (1,[2]) = [1;2] @>

[<Fact>]
let ``Shape F# Choice`` () =
    test <@ match shapeof<Choice<int,string>> with Shape.FSharpUnion u -> u.IsFSharpChoice | _ -> false @>
    let shape = shapeof<Choice<int, string>> |> unbox<IShapeFSharpUnion<Choice<int, string>, int, string>>
    test <@ shape.Project (Choice1Of2 2) = Choice1Of2 2 @>
    test <@ shape.Project (Choice2Of2 "42") = Choice2Of2 "42" @>
    test <@ shape.Construct1 2 = Choice1Of2 2 @>
    test <@ shape.Construct2 "2" = Choice2Of2 "2" @>