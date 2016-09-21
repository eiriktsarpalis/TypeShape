module TypeShape.Tests

open System
open System.Collections.Generic
open System.Reflection

open Xunit
open Swensen.Unquote.Assertions
open FsCheck

open TypeShape

open System.Runtime.Serialization
open TypeShape_ISerializableExtensions

let check<'T>(prop : 'T -> bool) = Check.QuickThrowOnFailure prop

let testPrim<'T>() = 
    let shape = shapeof<'T>
    test <@ shape.GetType() = typeof<TypeShape<'T>> @>
    let accepter = { new ITypeShapeVisitor<bool> with member __.Visit<'a>() = typeof<'T> = typeof<'a> }
    test <@ shape.Accept accepter @>

[<Fact>]
let ``Should fail on invalid type inputs`` () =
    raises<ArgumentNullException> <@ TypeShape.Create null @>
    raises<UnsupportedShape> <@ TypeShape.Create typedefof<int option> @>
    raises<UnsupportedShape> <@ TypeShape.Create (typedefof<int option>.GetGenericArguments().[0]) @>
    raises<UnsupportedShape> <@ TypeShape.Create (typeof<int>.MakeByRefType()) @>
    raises<UnsupportedShape> <@ TypeShape.Create (typeof<int>.MakePointerType()) @>
    match Type.GetType("System.__Canon") with
    | null -> ()
    | canon -> raises<UnsupportedShape> <@ TypeShape.Create canon @>

[<Fact>]
let ``Should correctly resolve untyped shapes`` () =
    test <@ TypeShape.Create typeof<int> :? TypeShape<int> @>
    test <@ TypeShape.Create typeof<string []> :? TypeShape<string []> @>
    test <@ TypeShape.Create typeof<int * string> :? TypeShape<int * string> @>
    test <@ TypeShape.Create typeof<BindingFlags> :? TypeShape<BindingFlags> @>

[<Fact>]
let ``Shape primitive`` () =
    testPrim<bool>() ; testPrim<byte>() ; testPrim<sbyte>()
    testPrim<int16>() ; testPrim<int32>() ; testPrim<int64>()
    testPrim<uint16>() ; testPrim<uint32>() ; testPrim<uint64>()

[<Fact>]
let ``Shape BCL primitives`` () =
    testPrim<DateTime>() ; testPrim<DateTimeOffset>()

type TypeWithDefaultCtor(x : int) =
    new () = new TypeWithDefaultCtor(42)
    member __.Value = x

[<Fact>]
let ``Shape Type with default ctor`` () =
    let accepter = 
        { new IDefaultConstructorVisitor<bool> with
            member __.Visit<'T when 'T : (new : unit -> 'T)> () = 
                let t = new 'T() :> obj :?> TypeWithDefaultCtor 
                t.Value = 42 }

    test <@ match shapeof<TypeWithDefaultCtor> with Shape.DefaultConstructor s -> s.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape Struct`` () =
    let accepter1 = 
        { new IStructVisitor<bool> with
            member __.Visit<'T when 'T : struct> () = true }
    let accepter2 = 
        { new INotStructVisitor<bool> with
            member __.Visit<'T when 'T : not struct and 'T : null> () = false }

    test <@ match shapeof<int> with Shape.Struct s -> s.Accept accepter1 | Shape.NotStruct s -> s.Accept accepter2 @>    
    test <@ not <| match shapeof<string> with Shape.Struct s -> s.Accept accepter1 | Shape.NotStruct s -> s.Accept accepter2 @>    

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
    let accepter e t =
        { new IEnumerableVisitor<bool> with
            member __.Visit<'E, 'T when 'E :> seq<'T>>() = typeof<'T> = t && typeof<'E> = e }
    test <@ match shapeof<int []> with Shape.Enumerable s -> s.Accept (accepter typeof<int []> typeof<int>) | _ -> false @>
    test <@ match shapeof<int list> with Shape.Enumerable s -> s.Accept (accepter typeof<int list> typeof<int>) | _ -> false @>
    test <@ match shapeof<seq<int>> with Shape.Enumerable s -> s.Accept (accepter typeof<seq<int>> typeof<int>) | _ -> false @>
    test <@ match shapeof<ResizeArray<int>> with Shape.Enumerable s -> s.Accept (accepter typeof<ResizeArray<int>> typeof<int>) | _ -> false @>
    test <@ match shapeof<HashSet<int>> with Shape.Enumerable s -> s.Accept (accepter typeof<HashSet<int>> typeof<int>) | _ -> false @>
    test <@ match shapeof<Dictionary<int, string>> with Shape.Enumerable s -> s.Accept (accepter typeof<Dictionary<int,string>> typeof<KeyValuePair<int, string>>) | _ -> false @>
    test <@ match shapeof<Set<int>> with Shape.Enumerable s -> s.Accept (accepter typeof<Set<int>> typeof<int>) | _ -> false @>
    test <@ match shapeof<Map<int, string>> with Shape.Enumerable s -> s.Accept (accepter typeof<Map<int,string>> typeof<KeyValuePair<int, string>>) | _ -> false @>
    test <@ match shapeof<IDictionary<int, string>> with Shape.Enumerable s -> true | _ -> false @>
    test <@ match shapeof<Stack<int>> with Shape.Enumerable s -> true | _ -> false @>

[<Fact>]
let ``Shape Collection`` () =
    let accepter c t =
        { new ICollectionVisitor<bool> with
            member __.Visit<'C, 'T when 'C :> ICollection<'T>>() = typeof<'T> = t && typeof<'C> = c}
    test <@ match shapeof<int []> with Shape.Collection s -> s.Accept (accepter typeof<int []> typeof<int>) | _ -> false @>
    test <@ match shapeof<seq<int>> with Shape.Collection s -> false | _ -> true @>
    test <@ match shapeof<ResizeArray<int>> with Shape.Collection s -> s.Accept (accepter typeof<ResizeArray<int>> typeof<int>) | _ -> false @>
    test <@ match shapeof<HashSet<int>> with Shape.Collection s -> s.Accept (accepter typeof<HashSet<int>> typeof<int>) | _ -> false @>
    test <@ match shapeof<Dictionary<int, string>> with Shape.Collection s -> s.Accept (accepter typeof<Dictionary<int, string>> typeof<KeyValuePair<int, string>>) | _ -> false @>
    test <@ match shapeof<Set<int>> with Shape.Collection s -> s.Accept (accepter typeof<Set<int>> typeof<int>) | _ -> false @>
    test <@ match shapeof<Map<int, string>> with Shape.Collection s -> s.Accept (accepter typeof<Map<int,string>> typeof<KeyValuePair<int, string>>) | _ -> false @>
    test <@ match shapeof<IDictionary<int, string>> with Shape.Collection s -> true | _ -> false @>

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
let ``Shape ISerializable`` () =
    let accepter =
        { new ISerializableVisitor<bool> with
            member __.Visit<'T when 'T :> ISerializable> () = typeof<'T> = typeof<exn> }

    test <@ match shapeof<exn> with Shape.ISerializable s -> s.Accept accepter | _ -> false @>

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
    let shape = 
        match shapeof<Record1> with
        | Shape.FSharpRecord1 (:? IShapeFSharpRecord<Record1, int> as r) -> r
        | _ -> raise <| new InvalidCastException()
        
    test <@ shape.Properties.Length = 1 @>
    check(fun (r:Record1) -> shape.Construct(shape.Project1 r) = r)
    check(fun (a1:int) -> shape.Project1(shape.Construct a1) = a1)

type Record2 = { A1 : int ; A2 : string }

[<Fact>]
let ``Shape Record 2`` () =
    test <@ match shapeof<Record2> with Shape.FSharpRecord2 _ -> true | _ -> false @>
    let shape = 
        match shapeof<Record2> with
        | Shape.FSharpRecord2 (:? IShapeFSharpRecord<Record2, int, string> as r) -> r
        | _ -> raise <| new InvalidCastException()

    test <@ shape.Properties.Length = 2 @>
    check(fun (r:Record2) -> shape.Construct(shape.Project1 r, shape.Project2 r) = r)
    check(fun (a1:int, a2:string) -> 
        let r = shape.Construct(a1, a2)
        shape.Project1 r = a1 && shape.Project2 r = a2)

type Record3 = { A1 : int ; A2 : string ; A3 : bool }

[<Fact>]
let ``Shape Record 3`` () =
    test <@ match shapeof<Record3> with Shape.FSharpRecord3 _ -> true | _ -> false @>
    let shape = 
        match shapeof<Record3> with
        | Shape.FSharpRecord3 (:? IShapeFSharpRecord<Record3, int, string, bool> as r) -> r
        | _ -> raise <| new InvalidCastException()

    test <@ shape.Properties.Length = 3 @>
    check(fun (r:Record3) -> shape.Construct(shape.Project1 r, shape.Project2 r, shape.Project3 r) = r)
    check(fun (a1:int, a2:string, a3:bool) -> 
        let r = shape.Construct(a1, a2, a3)
        shape.Project1 r = a1 && shape.Project2 r = a2 &&
        shape.Project3 r = a3)

type Record4 = { A1 : int ; A2 : string ; A3 : bool ; A4 : byte }

[<Fact>]
let ``Shape Record 4`` () =
    test <@ match shapeof<Record4> with Shape.FSharpRecord4 _ -> true | _ -> false @>
    let shape = 
        match shapeof<Record4> with
        | Shape.FSharpRecord4 (:? IShapeFSharpRecord<Record4, int, string, bool, byte> as r) -> r
        | _ -> raise <| new InvalidCastException()
    test <@ shape.Properties.Length = 4 @>
    check(fun (r:Record4) -> shape.Construct(shape.Project1 r, shape.Project2 r, shape.Project3 r, shape.Project4 r) = r)
    check(fun (a1:int, a2:string, a3:bool, a4:byte) -> 
        let r = shape.Construct(a1, a2, a3, a4)
        shape.Project1 r = a1 && shape.Project2 r = a2 &&
        shape.Project3 r = a3 && shape.Project4 r = a4)

type Record5 = { A1 : int ; A2 : string ; A3 : bool ; A4 : byte ; A5 : byte[] }

[<Fact>]
let ``Shape Record 5`` () =
    test <@ match shapeof<Record5> with Shape.FSharpRecord5 _ -> true | _ -> false @>
    let shape = 
        match shapeof<Record5> with
        | Shape.FSharpRecord5 (:? IShapeFSharpRecord<Record5, int, string, bool, byte, byte[]> as r) -> r
        | _ -> raise <| new InvalidCastException()
    test <@ shape.Properties.Length = 5 @>
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
    let shape = 
        match shapeof<Record6> with
        | Shape.FSharpRecord6 (:? IShapeFSharpRecord<Record6, int, string, bool, byte, byte[], decimal> as r) -> r
        | _ -> raise <| new InvalidCastException()

    test <@ shape.Properties.Length = 6 @>
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
    let shape = 
        match shapeof<Record7> with
        | Shape.FSharpRecord7 (:? IShapeFSharpRecord<Record7, int, string, bool, byte, byte[], decimal, int16> as r) -> r
        | _ -> raise <| new InvalidCastException()

    test <@ shape.Properties.Length = 7 @>
    check(fun (r:Record7) -> shape.Construct(shape.Project1 r, shape.Project2 r, shape.Project3 r, shape.Project4 r, shape.Project5 r, shape.Project6 r, shape.Project7 r) = r)
    check(fun (a1:int, a2:string, a3:bool, a4:byte, a5:byte[], a6:decimal, a7:int16) -> 
        let r = shape.Construct(a1, a2, a3, a4, a5, a6, a7)
        shape.Project1 r = a1 && shape.Project2 r = a2 &&
        shape.Project3 r = a3 && shape.Project4 r = a4 &&
        shape.Project5 r = a5 && shape.Project6 r = a6 &&
        shape.Project7 r = a7)

[<Fact>]
let ``Shape F# ref`` () =
    test <@ match shapeof<int ref> with Shape.FSharpRecord1 r -> r.Properties.Length = 1 | _ -> false @>
    let accepter = { new IFSharpRefVisitor<bool> with member __.Visit<'T>() = typeof<'T> = typeof<int> }
    test <@ match shapeof<int ref> with Shape.FSharpRef s -> s.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape F# choice 2`` () =
    let accepter = { new IFSharpChoice2Visitor<bool> with member __.Visit<'T1,'T2>() = typeof<'T1> = typeof<int> && typeof<'T2> = typeof<string> }
    test <@ match shapeof<Choice<int,string>> with Shape.FSharpChoice2 c -> c.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape F# choice 3`` () =
    let accepter = { new IFSharpChoice3Visitor<bool> with member __.Visit<'T1,'T2,'T3>() = typeof<'T1> = typeof<int> && typeof<'T2> = typeof<string> && typeof<'T3> = typeof<bool> }
    test <@ match shapeof<Choice<int,string,bool>> with Shape.FSharpChoice3 c -> c.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape F# choice 4`` () =
    let accepter = 
        { new IFSharpChoice4Visitor<bool> with 
            member __.Visit<'T1,'T2,'T3,'T4>() = 
                typeof<'T1> = typeof<int> && typeof<'T2> = typeof<string> && 
                typeof<'T3> = typeof<bool> && typeof<'T4> = typeof<byte[]> }
    test <@ match shapeof<Choice<int,string,bool,byte[]>> with Shape.FSharpChoice4 c -> c.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape F# choice 5`` () =
    let accepter = 
        { new IFSharpChoice5Visitor<bool> with 
            member __.Visit<'T1,'T2,'T3,'T4,'T5>() = 
                typeof<'T1> = typeof<int> && typeof<'T2> = typeof<string> && 
                typeof<'T3> = typeof<bool> && typeof<'T4> = typeof<byte[]> &&
                typeof<'T5> = typeof<sbyte> }
    test <@ match shapeof<Choice<int,string,bool,byte[],sbyte>> with Shape.FSharpChoice5 c -> c.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape F# choice 6`` () =
    let accepter = 
        { new IFSharpChoice6Visitor<bool> with 
            member __.Visit<'T1,'T2,'T3,'T4,'T5,'T6>() = 
                typeof<'T1> = typeof<int> && typeof<'T2> = typeof<string> && 
                typeof<'T3> = typeof<bool> && typeof<'T4> = typeof<byte[]> &&
                typeof<'T5> = typeof<sbyte> && typeof<'T6> = typeof<decimal> }
    test <@ match shapeof<Choice<int,string,bool,byte[],sbyte,decimal>> with Shape.FSharpChoice6 c -> c.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape F# choice 7`` () =
    let accepter = 
        { new IFSharpChoice7Visitor<bool> with 
            member __.Visit<'T1,'T2,'T3,'T4,'T5,'T6,'T7>() = 
                typeof<'T1> = typeof<int> && typeof<'T2> = typeof<string> && 
                typeof<'T3> = typeof<bool> && typeof<'T4> = typeof<byte[]> &&
                typeof<'T5> = typeof<sbyte> && typeof<'T6> = typeof<decimal> && typeof<'T7> = typeof<float> }
    test <@ match shapeof<Choice<int,string,bool,byte[],sbyte,decimal,float>> with Shape.FSharpChoice7 c -> c.Accept accepter | _ -> false @>

type Union1 = C1U1 of int * string

[<Fact>]
let ``Shape Union 1`` () =
    test <@ match shapeof<Union1> with Shape.FSharpUnion1 u -> u.UnionCaseInfo.Length = 1 | _ -> false @>
    let shape = 
        match shapeof<Union1> with 
        | Shape.FSharpUnion1 (:? IShapeFSharpUnion<Union1, int * string> as u) -> u
        | _ -> raise <| InvalidCastException()

    check(fun (u:Union1) -> shape.Construct1(shape.Project u) = u )

type Union2 = 
    | C1U2 of int * string
    | C2U2

[<Fact>]
let ``Shape Union 2`` () =
    test <@ match shapeof<Union2> with Shape.FSharpUnion2 u -> u.UnionCaseInfo.Length = 2 | _ -> false @>
    let shape = 
        match shapeof<Union2> with 
        | Shape.FSharpUnion2 (:? IShapeFSharpUnion<Union2, int * string, unit> as u) -> u
        | _ -> raise <| InvalidCastException()

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
    test <@ match shapeof<Union3> with Shape.FSharpUnion3 u -> u.UnionCaseInfo.Length = 3 | _ -> false @>
    let shape = 
        match shapeof<Union3> with 
        | Shape.FSharpUnion3 (:? IShapeFSharpUnion<Union3, int * string, unit, int> as u) -> u
        | _ -> raise <| InvalidCastException()

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
    test <@ match shapeof<Union4> with Shape.FSharpUnion4 u -> u.UnionCaseInfo.Length = 4 | _ -> false @>
    let shape = 
        match shapeof<Union4> with 
        | Shape.FSharpUnion4 (:? IShapeFSharpUnion<Union4, int * string, unit, int, unit> as u) -> u
        | _ -> raise <| InvalidCastException()

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
    test <@ match shapeof<Union5> with Shape.FSharpUnion5 u -> u.UnionCaseInfo.Length = 5 | _ -> false @>
    let shape = 
        match shapeof<Union5> with 
        | Shape.FSharpUnion5 (:? IShapeFSharpUnion<Union5, int * string, unit, int, unit, int * bool * byte[]> as u) -> u
        | _ -> raise <| InvalidCastException()

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
    test <@ match shapeof<Union6> with Shape.FSharpUnion6 u -> u.UnionCaseInfo.Length = 6 | _ -> false @>
    let shape = 
        match shapeof<Union6> with 
        | Shape.FSharpUnion6 (:? IShapeFSharpUnion<Union6, int * string, unit, int, unit, int * bool * byte[], string> as u) -> u
        | _ -> raise <| InvalidCastException()

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
    test <@ match shapeof<Union7> with Shape.FSharpUnion7 u -> u.UnionCaseInfo.Length = 7 | _ -> false @>
    let shape = 
        match shapeof<Union7> with 
        | Shape.FSharpUnion7 (:? IShapeFSharpUnion<Union7, int * string, unit, int, unit, int * bool * byte[], string, unit> as u) -> u
        | _ -> raise <| InvalidCastException()

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
    test <@ match shapeof<int option> with Shape.FSharpUnion _ -> true | _ -> false @>
    test <@ match shapeof<int option> with Shape.FSharpUnion2 _ -> true | _ -> false @>

[<Fact>]
let ``Shape F# list`` () =
    test <@ match shapeof<int list> with Shape.FSharpUnion2 _ -> true | _ -> false @>
    test <@ match shapeof<int list> with Shape.FSharpList _ -> true | _ -> false @>

[<Fact>]
let ``Shape F# Choice`` () =
    test <@ match shapeof<Choice<int,string>> with Shape.FSharpUnion2 _ -> true | _ -> false @>