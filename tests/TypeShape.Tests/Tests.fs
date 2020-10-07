module TypeShape.Tests.Tests

open System
open System.Collections.Generic
open System.Reflection
open System.Runtime.Serialization
open System.Runtime.CompilerServices

open FSharp.Reflection

open Xunit
open Swensen.Unquote.Assertions
open FsCheck

open TypeShape
open TypeShape.Core
open TypeShape.Core.Utils
open TypeShape.Core.SubtypeExtensions
open TypeShape.Empty
open TypeShape.Clone
open TypeShape.Tests.GenericTests

let check<'T>(prop : 'T -> bool) = Check.QuickThrowOnFailure prop
let checkCloner (cloner : 'T -> 'T) = check(fun t -> t = cloner t)
let inline refEq<'T when 'T : not struct> (x : 'T) (y : 'T) = obj.ReferenceEquals(x, y)

type Cycle = Cycle of Cycle
with static member Instance = let rec c = Cycle c in c

[<NoEquality; NoComparison>]
type NoEqNoComp = NoEqNoComp

let testPrim<'T>() = 
    let shape = shapeof<'T>
    test <@ shape.GetType() = typeof<TypeShape<'T>> @>
    let accepter = { new ITypeVisitor<bool> with member __.Visit<'a>() = typeof<'T> = typeof<'a> }
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

    match Type.GetType("System.Span`1") with
    | null -> ()
    | span -> 
        let instance = span.MakeGenericType(typeof<int>)
        raises<UnsupportedShape> <@ TypeShape.Create instance @>

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

    test <@ match shapeof<int> with Shape.Struct s -> s.Accept accepter1 | Shape.NotStruct s -> s.Accept accepter2 | _ -> false @>    
    test <@ not <| match shapeof<string> with Shape.Struct s -> s.Accept accepter1 | Shape.NotStruct s -> s.Accept accepter2 | _ -> false @>    

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

    let cloner = clone<Nullable<TimeSpan>>
    checkCloner cloner

    let stagedCloner = mkStagedCloner<Nullable<TimeSpan>>()
    checkCloner stagedCloner

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

    let cloner = clone<int * decimal * (string * int list) * bool * string option * uint64 * string * byte[] * string * byte[] * decimal>
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

    let cloner = clone<CSharpRecord>
    let target = cloner source
    test <@ obj.ReferenceEquals(source, target) |> not @>
    test <@ source = target @>
    test <@ source.GetterOnly <> target.GetterOnly @>

    let sCloner = clone<CSharpRecord>
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

    let cloner = clone<SimplePoco>
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
            member __.Visit<'exn when 'exn :> exn and 'exn : not struct and 'exn : null>() = typeof<'exn> = typeof<System.IO.FileNotFoundException> }
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
        { new ITypeVisitor<bool> with
            member __.Visit<'T> () = typeof<'T> = typeof<int> }

    test <@ match shapeof<int []>    with Shape.Array s when s.Rank = 1 -> s.Element.Accept accepter | _ -> false @>    
    test <@ match shapeof<int [,]>   with Shape.Array s when s.Rank = 2 -> s.Element.Accept accepter | _ -> false @>
    test <@ match shapeof<int [,,]>  with Shape.Array s when s.Rank = 3 -> s.Element.Accept accepter | _ -> false @>
    test <@ match shapeof<int [,,,]> with Shape.Array s when s.Rank = 4 -> s.Element.Accept accepter | _ -> false @>

[<Fact>]
let ``Shape ResizeArray`` () =
    let accepter = 
        { new ITypeVisitor<bool> with
            member __.Visit<'T>() = typeof<'T> = typeof<int> }

    test <@ match shapeof<ResizeArray<int>> with Shape.ResizeArray s -> s.Element.Accept accepter | _ -> false @>

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
    
    let cloner = clone<Exception>
    let exn' = cloner exn
    test <@ not (refEq exn exn') && exn.Message = exn'.Message @>

    let cloner' = mkStagedCloner<Exception>()
    let exn' = cloner' exn
    test <@ not (refEq exn exn') && exn.Message = exn'.Message @>


[<Fact>]
let ``Shape Subtype`` () =
    let (|ISerializable|_|) = Shape.tryCreateSubtypeShape<ISerializable>
    match shapeof<exn> with
    | ISerializable s -> s.Accept {
        new ISubtypeVisitor<ISerializable, bool> with
            member __.Visit<'S when 'S :> ISerializable>() = test <@ typeof<'S> = typeof<exn> @> ; true }
    | _ -> test <@ false @> ; true

[<Fact>]
let ``Shape F# Option`` () =
    let visitor ty =
        { new ITypeVisitor<bool> with member __.Visit<'T>() = typeof<'T> = ty }

    test <@ match shapeof<int option> with Shape.FSharpOption s -> s.Element.Accept (visitor typeof<int>) | _ -> false @>

[<Fact>]
let ``Shape F# list`` () =
    let visitor ty =
        { new ITypeVisitor<bool> with member __.Visit<'T>() = typeof<'T> = ty }

    test <@ match shapeof<int list> with Shape.FSharpList s -> s.Element.Accept (visitor typeof<int>) | _ -> false @>

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
    let cloner = clone<Record7>
    checkCloner cloner

    let scloner = mkStagedCloner<Record7>()
    checkCloner scloner


[<Fact>]
let ``Anonymous Records should be matched by the record active pattern`` () =
    test <@ match shapeof<{| x : int ; y : string ; z : bool|}> with Shape.FSharpRecord s -> s.IsAnonymousRecord && s.Fields.Length = 3 | _ -> false @>

[<Fact>]
let ``Anonymous Record cloning`` () =
    let cloner = clone< {| x : int ; y : string; z : bool; w : int list|} >
    checkCloner cloner

    let scloner = mkStagedCloner< {| x : int ; y : string; z : bool; w : int list|} >()
    checkCloner scloner

[<Fact>]
let ``Anonymous struct Records should be matched by the record active pattern`` () =
    test <@ match shapeof<struct {| x : int ; y : string ; z : bool|}> with Shape.FSharpRecord s -> s.IsAnonymousRecord && s.Fields.Length = 3 | _ -> false @>

[<Fact>]
let ``Anonymous struct Record cloning`` () =
    let cloner = clone<struct {| x : int ; y : string; z : bool; w : int list|} >
    checkCloner cloner

    let scloner = mkStagedCloner< struct {| x : int ; y : string; z : bool; w : int list|} >()
    checkCloner scloner

[<Fact>]
let ``Shape F# ref`` () =
    test <@ match shapeof<int ref> with Shape.FSharpRecord r -> r.Fields.Length = 1 | _ -> false @>
    let accepter = { new ITypeVisitor<bool> with member __.Visit<'T>() = typeof<'T> = typeof<int> }
    test <@ match shapeof<int ref> with Shape.FSharpRef s -> s.Element.Accept accepter | _ -> false @>

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

    let cloner = clone<Union7>
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
    let cloner = clone<P>
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

    let cloner = clone<StructRecord>
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

    let cloner = clone<StructUnion>
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

        let cloner = clone<'STuple>
        test <@ stuple = cloner stuple @>

        let scloner = mkStagedCloner<'STuple>()
        test <@ stuple = scloner stuple @>


    testStructTuple (struct(1,"2"))
    testStructTuple (struct(1,"2",3,"4",5,"6",7,"8",9,"10",11))
    testStructTuple (struct(1,"2",3,"4",5,"6",7,"8",9,"10",11,"12",13,"14",15,"16",17,"18"))

type MixedCase = | One of OneId:int
[<Fact>]
let ``Should support union with mixed case properties``() =
    match shapeof<MixedCase> with
    | Shape.FSharpUnion (:? ShapeFSharpUnion<MixedCase> as shape) ->
        test <@ shape.UnionCases.Length = 1 @>
        test <@ shape.UnionCases.[0].Fields.[0].Label = "OneId" @>
    | _ -> failwith "Unexpected shape"

[<Fact>]
let ``Clone should support cyclic object graphs``() =
    let cyclicArray =
        let xs = Array.zeroCreate<obj> 10
        for i = 0 to xs.Length - 1 do xs.[i] <- box xs
        xs

    let clonedArray = clone cyclicArray

    test <@ obj.ReferenceEquals(clonedArray, cyclicArray) |> not @>
    test <@ clonedArray |> Array.forall (fun x -> obj.ReferenceEquals(x,clonedArray)) @>


// Bug https://github.com/eiriktsarpalis/TypeShape/issues/14
type NamedDUData = NamedDUData of AA: string

[<Fact>]
let ``Should successfully work on union shapes with uppercase labels``() =
    let x = NamedDUData(AA = "foo")
    test <@ clone x = x @>


type Empty<'T> = Empty of 'T

[<Fact>]
let ``Empty should update definitions on new registrations`` () =
    TypeShape.Empty.register (fun () -> Empty 42)
    TypeShape.Empty.register (fun () -> Empty "string")

    test <@ empty<Empty<int> * Empty<string>> = (Empty 42, Empty "string") @>

    TypeShape.Empty.register (fun () -> Empty -1)
    TypeShape.Empty.register (fun () -> Empty "otherString")

    test <@ empty<Empty<int> * Empty<string>> = (Empty -1, Empty "otherString") @>


[<Fact>]
let ``Shape-Poco should not match primitives`` () =
    test 
        <@
            match shapeof<int> with
            | Shape.Poco _ -> false
            | _ -> true
        @>

[<Fact>]
let ``Shape-Poco should not match enums`` () =
    test 
        <@
            match shapeof<System.Reflection.BindingFlags> with
            | Shape.Poco _ -> false
            | _ -> true
        @>

[<Fact>]
let ``Shape-Poco should not match interfaces`` () =
    test 
        <@
            match shapeof<ITypeVisitor<int>> with
            | Shape.Poco _ -> false
            | _ -> true
        @>

#if !NET5_0
[<Fact>]
let ``Shape-Poco should handle write-only properties`` () =
    // https://github.com/eiriktsarpalis/TypeShape/issues/23
    test
        <@
            match shapeof<System.Security.PermissionSet> with
            | Shape.Poco _ -> true
            | _ -> false
        @>

[<Fact>]
let ``Shape-Poco should not match MarshalByRef types`` () =
    test 
        <@
            match shapeof<System.MarshalByRefObject> with
            | Shape.Poco _ -> false
            | _ -> true
        @>
#endif

[<Fact>]
let ``Shape-Poco should handle string correctly`` () =
    
    test 
        <@
            match shapeof<string> with
            | Shape.Poco _ -> true
            | _ -> false
        @>

#if NETCOREAPP
type [<IsByRefLike>] Foo = struct end
type Bar() = member _.Foo =  Foo()
type Baz(foo: Foo) = class end

[<Fact>]
let ``Shape-Poco should not match types with ByRefLike props or fields`` () =
    test <@ match shapeof<Bar> with
            | Shape.Poco _ -> false
            | _ -> true @>

[<Fact>]
let ``Shape-Poco should not handle ctor with ByRefLike args`` () =
    match shapeof<Baz> with
    | Shape.Poco s -> test <@ s.Constructors.Length = 0 @>
    | _ -> ()
#endif

module GenericClone =

    [<Fact>]
    let ``Generic Clone should produde equal values`` () =
        { new Predicate with 
            member __.Invoke (t : 'T) = t = clone(t) }
        |> Check.GenericPredicate false false 100 10

module GenericCloneStaged =

    [<Fact>]
    let ``Generic Staged Clone should produde equal values`` () =
        { new Predicate with 
            member __.Invoke (t : 'T) = 
                let c = mkStagedCloner<'T>()
                c(t) = t }
        |> Check.GenericPredicate false false 100 1

module GenericCloneHKT =

    [<Fact>]
    let ``HKT-clone should produce equal values`` () =
        { new Predicate with
            member __.Invoke (t : 'T) =
                let c = HktClone.mkCloner<'T>()
                if c(t) <> t then failwithf "%A != %A" (c t) t else true}
        |> Check.GenericPredicate false false 100 10

module GenericEmpty =

    [<Fact>]
    let ``Empty should always produce equal values`` () =
        { new Predicate with 
            member __.Invoke (t : 'T) = 
                empty<'T> = empty<'T> }
        |> Check.GenericPredicate false false 100 10

module ``Generic SizeOf`` =

    open TypeShape.Sizeof

    [<Fact>]
    let ``Sizeof should terminate for all inputs`` () =
        { new Predicate with 
            member __.Invoke (t : 'T) = gsizeof t >= 0L }
        |> Check.GenericPredicate false false 100 10

    [<Fact>]
    let ``Sizeof should support cyclic objects`` () =
        test <@ gsizeof Cycle.Instance >= 0L @>

module ``Generic Combinators`` =

    [<Fact>]
    let ``Generic map should work as expected`` () =
        check<int list list>(fun xs ->
            let expected = xs |> List.map (List.map ((+) 1))
            let actual = Generic.map ((+) 1) xs
            expected = actual)

    [<Fact>]
    let ``Generic map should support generic types`` () =
        { new Predicate with 
            member __.Invoke (t : 'T) = 
                let _ = Generic.map ((+) 1) t in true }
        |> Check.GenericPredicate false false 100 10

    [<Fact>]
    let ``Generic summation`` () =
        check<int list list> (fun xs ->
            let expected = xs |> Seq.concat |> Seq.sum
            let actual = Generic.sumBy id (Some [|xs|])
            expected = actual)

    [<Fact>]
    let ``Generic fold should support generic types`` () =
        { new Predicate with 
            member __.Invoke (t : 'T) = 
                Generic.fold (fun c _ -> c + 1) 0 t >= 0 }
        |> Check.GenericPredicate false false 100 10

    [<Fact>]
    let ``Generic fold should support generic cyclic objects`` () =
        test <@ Generic.fold (fun c _ -> c + 1) 0 Cycle.Instance = 1 @>

    [<Fact>]
    let ``Generic exists should work as expected`` () =
        let waldo = Some [Some "joe" ; None ; Some "bill" ; Some "waldo"] |> ref
        let noWaldo = Some [Some "joe" ; None ; Some "bill" ; Some "peter"] |> ref
        test <@ Generic.exists ((=) "waldo") waldo @>
        test <@ Generic.exists ((=) "waldo") noWaldo |> not @>

    [<Fact>]
    let ``Generic forall should work as expected`` () =
        let allWaldo = Some [Some "waldo" ; None ; Some "waldo" ; Some "waldo"] |> ref
        let notAllWaldo = Some [Some "waldo" ; None ; Some "waldo" ; Some "peter"] |> ref
        test <@ Generic.forall ((=) "waldo") allWaldo @>
        test <@ Generic.forall ((=) "waldo") notAllWaldo |> not @>

    [<Fact>]
    let ``Generic exists should cancel on falsification`` () =
        let values = [ for i in 1 .. 200 -> Some i]
        let counter = ref 0
        test <@ Generic.exists (fun i -> incr counter; i = 100) values @>
        test <@ !counter = 100 @>

    [<Fact>]
    let ``Generic forall should cancel on falsification`` () =
        let values = [ for i in 1 .. 200 -> Some i]
        let counter = ref 0
        test <@ Generic.forall (fun i -> incr counter; i < 100) values |> not @>
        test <@ !counter = 100 @>