module TypeShape.Tests.Tests

open System
open System.Collections.Generic
open System.Reflection
open System.Runtime.Serialization

open FSharp.Reflection

open Xunit
open Swensen.Unquote.Assertions
open FsCheck

open TypeShape
open TypeShape_Utils

let check<'T>(prop : 'T -> bool) = Check.QuickThrowOnFailure prop
let checkCloner (cloner : 'T -> 'T) = check(fun t -> t = cloner t)
let inline refEq<'T when 'T : not struct> (x : 'T) (y : 'T) = obj.ReferenceEquals(x, y)

[<NoEquality; NoComparison>]
type NoEqNoComp = NoEqNoComp

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
            member __.Visit<'T, 'U when 'T : enum<'U>
                                    and 'T : struct
                                    and 'T :> ValueType
                                    and 'T : (new : unit -> 'T)>() = 
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
let ``Shape Equality`` () =
    let testType expected (t:Type) =
        match TypeShape.Create t with
        | Shape.Equality s ->
            if not expected then false else
            s.Accept { new IEqualityVisitor<bool> with
                        member __.Visit<'T when 'T : equality> () = typeof<'T> = t }

        | _ -> not expected

    test <@ typeof<int> |> testType true @>
    test <@ typeof<string> |> testType true @>
    test <@ typeof<string * int option> |> testType true @>
    test <@ typeof<Type list> |> testType true @>
    test <@ typeof<string []> |> testType true @>
    test <@ typeof<obj> |> testType true @>

    test <@ typeof<NoEqNoComp> |> testType false @>
    test <@ typeof<NoEqNoComp option> |> testType false @>
    test <@ typeof<NoEqNoComp ref> |> testType false @>
    test <@ typeof<NoEqNoComp []> |> testType false @>
    test <@ typeof<NoEqNoComp list> |> testType false @>
    test <@ typeof<NoEqNoComp * int> |> testType false @>
    test <@ typeof<int -> int> |> testType false @>

[<Fact>]
let ``Shape Comparison`` () =
    let testType expected (t:Type) =
        match TypeShape.Create t with
        | Shape.Comparison s ->
            if not expected then false else
            s.Accept { new IComparisonVisitor<bool> with
                member __.Visit<'T when 'T : comparison>() = typeof<'T> = t }

        | _ -> not expected

    test <@ typeof<int> |> testType true @>
    test <@ typeof<string> |> testType true @>
    test <@ typeof<IntPtr> |> testType true @>
    test <@ typeof<string * int option> |> testType true @>
    test <@ typeof<string []> |> testType true @>
    test <@ typeof<string list> |> testType true @>
    test <@ typeof<string ref> |> testType true @>

    test <@ typeof<obj> |> testType false @>
    test <@ typeof<Type> |> testType false @>
    test <@ typeof<Type ref list option []> |> testType false @>
    test <@ typeof<NoEqNoComp> |> testType false @>
    test <@ typeof<NoEqNoComp option> |> testType false @>
    test <@ typeof<NoEqNoComp ref> |> testType false @>
    test <@ typeof<NoEqNoComp []> |> testType false @>
    test <@ typeof<NoEqNoComp list> |> testType false @>
    test <@ typeof<NoEqNoComp * int> |> testType false @>
    test <@ typeof<int -> int> |> testType false @>

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
let ``Shape Generic Tuple`` () =
    let checkShape tupleType =
        match TypeShape.Create tupleType with
        | Shape.Tuple shape ->
            shape.Accept { new ITupleVisitor<bool> with
                member __.Visit (tuple : ShapeTuple<'Tuple>) =
                    typeof<'Tuple> = tupleType &&
                    tuple.Elements.Length = FSharpType.GetTupleElements(tupleType).Length }

        | _ -> failwithf "Shape %O not recognized as a tuple" tupleType

    test <@ checkShape typeof<Tuple<string>> @>
    test <@ checkShape typeof<int * string> @> 
    test <@ checkShape typeof<int * decimal * byte[] * bigint> @>
    test <@ checkShape typeof<int * int * int * int * int * int * int * int * int * int * int * int> @>

    let cloner = mkCloner<int * decimal * (string * int list) * bool * string option * uint64 * string * byte[] * string * byte[] * decimal>()
    checkCloner cloner
    let scloner = mkStagedCloner<int * decimal * (string * int list) * bool * string option * uint64 * string * byte[] * string * byte[] * decimal>()
    checkCloner scloner

type CSharpRecord() =
    static let mutable counter = 0
    let count = System.Threading.Interlocked.Increment &counter
    member val Foo = "" with get,set
    member val Bar = false with get,set
    member val Baz = 0 with get,set
    member val TimeSpan = TimeSpan.Zero with get,set

    member __.GetterOnly = count

    override x.Equals y =
        match y with
        | :? CSharpRecord as y -> 
            x.Foo = y.Foo && x.Bar = y.Bar && 
            x.Baz = y.Baz && x.TimeSpan = y.TimeSpan
        | _ -> false

    override x.GetHashCode() = hash(x.Foo,x.Bar,x.Baz,x.TimeSpan)

[<Fact>]
let ``Shape CliMutable`` () =
    match TypeShape.Create<CSharpRecord>() with
    | Shape.CliMutable r -> 
        r.Accept { new ICliMutableVisitor<bool> with
            member __.Visit (shape : ShapeCliMutable<'R>) =
                test <@ typeof<'R> = typeof<CSharpRecord> @>
                test <@ shape.Properties.Length = 4 @>
                true }
    | _ -> failwithf "Type %O not recognized as C# record" typeof<CSharpRecord>
    |> ignore

    let source = new CSharpRecord(Foo = "Foo", Bar = true, Baz = 42, TimeSpan = TimeSpan.MaxValue)

    let cloner = mkCloner<CSharpRecord>()
    let target = cloner source
    test <@ obj.ReferenceEquals(source, target) |> not @>
    test <@ source = target @>
    test <@ source.GetterOnly <> target.GetterOnly @>

    let sCloner = mkCloner<CSharpRecord> ()
    let target = sCloner source
    test <@ obj.ReferenceEquals(source, target) |> not @>
    test <@ source = target @>
    test <@ source.GetterOnly <> target.GetterOnly @>

type SimplePoco(x : string, y : int) =
    static let staticField = 42
    member __.X = x
    member __.Y = y

[<Fact>]
let ``Shape Poco`` () =
    match TypeShape.Create<SimplePoco>() with
    | Shape.Poco s ->
        s.Accept { new IPocoVisitor<bool> with
            member __.Visit (shape : ShapePoco<'P>) =
                test <@ typeof<'P> = typeof<SimplePoco> @>
                test <@ shape.Fields.Length = 2 @>
                test <@ shape.Properties.Length = 2 @>
                test <@ shape.Constructors.Length = 1 @>
                true }
        |> ignore

    | _ -> failwithf "Type %O not recognized as POCO" typeof<SimplePoco>

    let source = new SimplePoco("foo", 42)

    let cloner = mkCloner<SimplePoco>()
    let target = cloner source
    test <@ obj.ReferenceEquals(source, target) |> not @>
    test <@ source.X = target.X @>
    test <@ source.Y = target.Y @>

    let scloner = mkStagedCloner<SimplePoco>()
    let target = scloner source
    test <@ obj.ReferenceEquals(source, target) |> not @>
    test <@ source.X = target.X @>
    test <@ source.Y = target.Y @>

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
    let accepter rk = 
        { new IArrayVisitor<bool> with
            member __.Visit<'T> rank = typeof<'T> = typeof<int> && rank = rk }

    test <@ match shapeof<int []> with Shape.Array s -> s.Accept (accepter 1) | _ -> false @>    
    test <@ match shapeof<int [,]> with Shape.Array s -> s.Accept (accepter 2) | _ -> false @>
    test <@ match shapeof<int [,,]> with Shape.Array s -> s.Accept (accepter 3) | _ -> false @>
    test <@ match shapeof<int [,,,]> with Shape.Array s -> s.Accept (accepter 4) | _ -> false @>

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
            member __.Visit<'T when 'T :> ISerializable> (_ : ShapeISerializable<'T>) = typeof<'T> = typeof<exn> }

    test <@ match shapeof<exn> with Shape.ISerializable s -> s.Accept accepter | _ -> false @>

    let exn = new Exception("kaboom!")
    
    let cloner = mkCloner<Exception>()
    let exn' = cloner exn
    test <@ not (refEq exn exn') && exn.Message = exn'.Message @>

    let cloner' = mkStagedCloner<Exception>()
    let exn' = cloner' exn
    test <@ not (refEq exn exn') && exn.Message = exn'.Message @>


[<Fact>]
let ``Shape F# Option`` () =
    let visitor ty =
        { new IFSharpOptionVisitor<bool> with member __.Visit<'T>() = typeof<'T> = ty }

    test <@ match shapeof<int option> with Shape.FSharpOption s -> s.Accept (visitor typeof<int>) | _ -> false @>

[<Fact>]
let ``Shape F# list`` () =
    let visitor ty =
        { new IFSharpListVisitor<bool> with member __.Visit<'T>() = typeof<'T> = ty }

    test <@ match shapeof<int list> with Shape.FSharpList s -> s.Accept (visitor typeof<int>) | _ -> false @>

[<Fact>]
let ``Shape F# Map`` () =
    let accepter = 
        { new IFSharpMapVisitor<bool> with
            member __.Visit<'K, 'V when 'K : comparison>() = typeof<'K> = typeof<string> && typeof<'V> = typeof<int> }

    test <@ match shapeof<Map<string, int>> with Shape.FSharpMap s -> s.Accept accepter | _ -> false @>


type Record7 = 
    { 
        A1 : int ; A2 : string ; A3 : bool ; A4 : byte ; A5 : byte[] ; A6 : decimal ; A7 : int16 
    }

[<Fact>]
let ``Shape Record 7`` () =
    let shape = 
        match shapeof<Record7> with
        | Shape.FSharpRecord (:? ShapeFSharpRecord<Record7> as r) -> r
        | _ -> raise <| new InvalidCastException()

    test <@ shape.Fields.Length = 7 @>
    let cloner = Clone.mkCloner<Record7>()
    checkCloner cloner

    let scloner = mkStagedCloner<Record7>()
    checkCloner scloner

[<Fact>]
let ``Shape F# ref`` () =
    test <@ match shapeof<int ref> with Shape.FSharpRecord r -> r.Fields.Length = 1 | _ -> false @>
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

type Union7 = 
    | C1U7 of int * _tag:string
    | C2U7
    | C3U7 of int
    | C4U7 of unit
    | C5U7 of int * bool * byte[]
    | C6U7 of string
    | C7U7

[<Fact>]
let ``Shape Union 7`` () =
    let shape = 
        match shapeof<Union7> with 
        | Shape.FSharpUnion s -> 
            s.Accept { new IFSharpUnionVisitor<bool> with
                member __.Visit (shape : ShapeFSharpUnion<'U>) =
                    test <@ typeof<'U> = typeof<Union7> @>
                    test <@ shape.UnionCases.Length = 7 @>
                    true
            }

        | _ -> raise <| InvalidCastException()

    let cloner = mkCloner<Union7>()
    checkCloner cloner

    let scloner = mkStagedCloner<Union7>()
    checkCloner scloner


[<Fact>]
let ``Shape F# Option as union`` () =
    test <@ match shapeof<int option> with Shape.FSharpUnion u -> u.UnionCases.Length = 2 | _ -> false @>

[<Fact>]
let ``Shape F# list as union`` () =
    test <@ match shapeof<int list> with Shape.FSharpUnion u -> u.UnionCases.Length = 2 | _ -> false @>

[<Fact>]
let ``Shape F# Choice as union`` () =
    test <@ match shapeof<Choice<int,string,bool>> with Shape.FSharpUnion u -> u.UnionCases.Length = 3 | _ -> false @>


type P = Z | S of P

[<Fact>]
let ``Should clone recursive types`` () =
    let cloner = mkCloner<P>()
    checkCloner cloner

[<Fact>]
let ``BinSearch should report correct indices`` () =
    let property (inputs : string []) =
        let inputs = Array.distinct inputs
        let binSearch = BinSearch inputs

        test 
            <@ 
                inputs 
                |> Seq.mapi (fun i v -> i,v)
                |> Seq.forall (fun (i,v) -> binSearch.TryFindIndex v = i)
            @>

    Check.QuickThrowOnFailure property


[<Fact>]
let ``BinSearch should return -1 on non-existingValues`` () =
    let property (inputs : Set<string>) (otherValues : Set<string>) =
        let binSearch = BinSearch (Set.toArray inputs)
        let missingValues = otherValues - inputs
        test 
            <@
                missingValues
                |> Seq.forall (fun v -> binSearch.TryFindIndex v = -1)
            @>

    Check.QuickThrowOnFailure property

[<Struct>]
type StructRecord = { A : int ; B : string }

[<Fact>]
let ``Should support struct records``() =
    match shapeof<StructRecord> with
    | Shape.FSharpRecord (:? ShapeFSharpRecord<StructRecord> as s) ->
        test <@ s.IsStructRecord && s.Fields.Length = 2 @>
    | _ -> raise <| InvalidCastException()

    let cloner = Clone.mkCloner<StructRecord>()
    checkCloner cloner

    let scloner = mkStagedCloner<StructRecord>()
    checkCloner scloner

[<Struct>]
type StructUnion = 
    | SU1 of a:int
    //| SU2 of b:int
    | SU3
    | SU4 of string
    | SU5 of byte[] * int64
    //| SU6

[<Fact>]
let ``Should support struct unions``() =
    match shapeof<StructUnion> with
    | Shape.FSharpUnion (:? ShapeFSharpUnion<StructUnion> as s) ->
        test <@ s.IsStructUnion && s.UnionCases.Length = 4 (* 6 *) @>
        let fieldTypes = s.UnionCases |> Array.map (fun c -> c.Fields |> Array.map (fun f -> f.Member.Type))
        test <@ fieldTypes = 
                    [|
                        [|typeof<int>|]
                        //[|typeof<int>|]
                        [||]
                        [|typeof<string>|];
                        [|typeof<byte[]>;typeof<int64>|]
                        //[||]
                    |] 
             @>

    | _ -> raise <| InvalidCastException()

    let cloner = Clone.mkCloner<StructUnion>()
    checkCloner cloner

    let scloner = mkStagedCloner<StructUnion>()
    checkCloner scloner

[<Fact>]
let ``Should support struct tuples``() =
    let testStructTuple (stuple : 'STuple) =
        let elems = FSharp.Reflection.FSharpType.GetTupleElements typeof<'STuple>
        match shapeof<'STuple> with
        | Shape.Tuple (:? ShapeTuple<'STuple> as s) ->
            test <@ s.IsStructTuple && s.Elements |> Array.map (fun e -> e.Member.Type) = elems @>
        | _ -> raise <| InvalidCastException()

        let cloner = Clone.mkCloner<'STuple>()
        test <@ stuple = cloner stuple @>

        let scloner = mkStagedCloner<'STuple>()
        test <@ stuple = scloner stuple @>


    testStructTuple (struct(1,"2"))
    testStructTuple (struct(1,"3",2,"4",5,"5",6,"7",8,"9",10))
    testStructTuple (struct(1,"3",2,"4",5,"5",6,"7",8,"9",10,"11",12,"13",14,"15",16,"17"))