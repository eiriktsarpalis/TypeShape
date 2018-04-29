#r "../src/TypeShape/bin/Release/net45/TypeShape.dll"

// A toy object mocking implementation

open System
open System.Collections.Generic
open TypeShape.Core
open TypeShape.Core.Utils

/// Attribute used for customizing member mocks
type MockAttribute() =
    inherit Attribute()
    /// Set prefered value to be used for member
    member val Value = null with get, set
    /// Set prefered size to be used for members of variadic types
    /// like nullables, optionals, lists and arrays
    member val Size = -1 with get, set
    /// Set prefered union case tag to be used for DU members
    member val PreferedUnionCase = -1 with get, set

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type private MockContext<'T> =
    | Empty
    | PreferedUnionCase of int
    | CustomLength of length:int
    | Static of value:'T
    | Variadic of value:obj
    | VariadicCustomLength of value:obj * length:int

type private Mocker<'T> = MockContext<'T> -> 'T

let rec private mkMocker<'T> () : Mocker<'T> =
    let mutable mock = Unchecked.defaultof<Mocker<'T>>
    if cache.TryGetValue(&mock) then mock
    else
        use ctx = cache.CreateGenerationContext()
        mkMockerCached ctx

and private mkMockerCached<'T> (ctx : TypeGenerationContext) : Mocker<'T> =
    match ctx.InitOrGetCachedValue<Mocker<'T>>(fun c m -> c.Value m) with
    | Cached(value = m) -> m
    | NotCached t ->
        let m = mkMockerAux<'T> ctx
        ctx.Commit t m

and private mkMockerAux<'T> (ctx : TypeGenerationContext) : Mocker<'T> =
    let EQ (mocker : Mocker<'a>) : Mocker<'T> = unbox mocker

    let inline getPrimMock (mv : MockContext<'a>) (defaultV : 'a) =
        match mv with Static t -> t | _ -> defaultV

    let mkMemberMocker (shape : IShapeWriteMember<'T>) =
        let mockValue : (obj * int * int) option =
            shape.MemberInfo.GetCustomAttributes(true)
            |> Seq.tryPick(function :? MockAttribute as m -> Some (m.Value, m.Size, m.PreferedUnionCase) | _ -> None)

        let invalidMock v =
            sprintf "Member '%O' contains mock attribute with incompatible value '%O'." 
                shape.MemberInfo v
            |> invalidArg (string typeof<'T>)

        shape.Accept { new IWriteMemberVisitor<'T, 'T -> 'T> with
            member __.Visit (shape : ShapeWriteMember<'T, 'Field>) =
                let mockValue =
                    match mockValue with
                    | Some((:? 'Field as f), _, _) -> Static f
                    | Some(_,_,tag) when tag >= 0 ->
                        match shapeof<'Field> with
                        | Shape.FSharpUnion u ->
                            if tag < u.UnionCases.Length then PreferedUnionCase tag
                            else
                                sprintf "argument '%d' exceeds union '%O' arity" tag typeof<'Field>
                                |> invalidArg "PreferedUnionCase"
                        | _ -> 
                            sprintf "Applied to type '%O' which is not a union" typeof<'Field>
                            |> invalidArg "PreferedUnionCase"

                    | Some(null, length, _) when length < 0 -> Empty
                    | Some(null, length, _) when length >= 0 -> CustomLength length
                    | Some (o, length, _) ->
                        let fieldShape = shapeof<'Field>
                        let mockShape = TypeShape.FromValue o
                        let mkVariadic() =
                            if length >= 0 then VariadicCustomLength(o,length)
                            else Variadic o

                        match fieldShape with
                        | Shape.FSharpOption s when s.Element = mockShape -> mkVariadic()
                        | Shape.Nullable s when s.Element = mockShape -> mkVariadic()
                        | Shape.FSharpList s when s.Element = mockShape -> mkVariadic()
                        | Shape.Array s when s.Rank = 1 && s.Element = mockShape -> mkVariadic()
                        | Shape.FSharpSet s  when s.Element = mockShape -> mkVariadic()
                        | _ -> invalidMock o

                    | None -> Empty

                let fm = mkMockerCached<'Field> ctx
                fun target -> shape.Inject target (fm mockValue) }

    match shapeof<'T> with
    | Shape.Primitive -> fun mv -> getPrimMock mv Unchecked.defaultof<'T>
    | Shape.Decimal -> EQ(fun mv -> getPrimMock mv 0M)
    | Shape.BigInt -> EQ(fun mv -> getPrimMock mv 0I)
    | Shape.Guid -> EQ(fun mv -> getPrimMock mv Guid.Empty)
    | Shape.TimeSpan -> EQ(fun mv -> getPrimMock mv TimeSpan.Zero)
    | Shape.DateTime -> EQ(fun mv -> getPrimMock mv DateTime.MinValue)
    | Shape.DateTimeOffset -> EQ(fun mv -> getPrimMock mv DateTimeOffset.MinValue)
    | Shape.Unit -> EQ(fun _ -> ())
    | Shape.String -> EQ(fun mv -> getPrimMock mv "")
    | Shape.Enum s ->
        s.Accept { new IEnumVisitor<Mocker<'T>> with
            member __.Visit<'t, 'u when 't : enum<'u>
                                    and 't : struct
                                    and 't :> ValueType
                                    and 't : (new : unit -> 't)>() = // 'T = 't
                let em = mkMockerCached<'u> ctx
                fun size mv -> 
                    match mv with
                    | Static t -> t
                    | _ -> LanguagePrimitives.EnumOfValue<'u,'t>(em Empty)
                |> EQ }

    | Shape.Nullable s ->
        s.Accept { new INullableVisitor<Mocker<'T>> with
            member __.Visit<'t when 't : struct and 't :> ValueType and 't : (new : unit -> 't)>() = // 'T = Nullable<'t>
                let tm = mkMockerCached<'t> ctx
                fun mv ->
                    match mv with
                    | Static t -> t
                    | Variadic(:? 't as t) -> Nullable<'t>(t)
                    | VariadicCustomLength(:? 't as t, l) when l > 0 -> Nullable<'t>(t)
                    | CustomLength l when l > 0 -> Nullable<'t>(tm Empty)
                    | Empty -> Nullable<'t>(tm Empty)
                    | _ -> Nullable<'t>()
                |> EQ }

    | Shape.FSharpFunc s ->
        // mock<'T -> 'S> = fun (_ : 'T) -> mock<'S>
        s.Accept { new IFSharpFuncVisitor<Mocker<'T>> with
            member __.Visit<'Dom, 'Cod> () = // 'T = 'Cod -> 'Dom
                let dm = mkMockerCached<'Cod> ctx
                fun mv ->
                    match mv with
                    | Static t -> t
                    | _ -> fun (_ : 'Dom) -> dm Empty
                |> EQ }

    | Shape.FSharpOption s ->
        s.Accept { new IFSharpOptionVisitor<Mocker<'T>> with
            member __.Visit<'t>() = // 'T = 't option
                let em = mkMockerCached<'t> ctx
                fun mv ->
                    match mv with
                    | Static t -> t
                    | Variadic(:? 't as t) -> Some t
                    | VariadicCustomLength(:? 't as t, l) when l > 0 -> Some t
                    | CustomLength l when l > 0 -> Some (em Empty)
                    | Empty -> Some(em Empty)
                    | _ -> None
                |> EQ }

    | Shape.KeyValuePair s ->
        s.Accept { new IKeyValuePairVisitor<Mocker<'T>> with
            member __.Visit<'k,'v>() = // 'T = KeyValuePair<'k,'v>
                let km,vm = mkMockerCached<'k> ctx, mkMockerCached<'v> ctx
                fun mv ->
                    match mv with
                    | Static t -> t
                    | _ -> new KeyValuePair<'k,'v>(km Empty, vm Empty)
                |> EQ }

    | Shape.Array s when s.Rank = 1 ->
        s.Accept { new IArrayVisitor<Mocker<'T>> with
            member __.Visit<'t> _ = // 'T = 't []
                let em = mkMockerCached<'t> ctx
                fun mv ->
                    match mv with
                    | Static t -> t
                    | Variadic(:? 't as t) -> [| t |]
                    | VariadicCustomLength(:? 't as t, l) when l > 0 -> Array.init l (fun _ -> t)
                    | CustomLength l when l > 0 -> Array.init l (fun _ -> em Empty)
                    | Empty -> [| em Empty |]
                    | _ -> Array.empty<'t>
                |> EQ }

    | Shape.FSharpList s ->
        s.Accept { new IFSharpListVisitor<Mocker<'T>> with
            member __.Visit<'t>() = // 'T = 't list
                let em = mkMockerCached<'t> ctx
                fun mv ->
                    match mv with
                    | Static t -> t
                    | Variadic(:? 't as t) -> [ t ]
                    | VariadicCustomLength(:? 't as t, l) when l > 0 -> List.init l (fun _ -> t)
                    | CustomLength l when l > 0 -> List.init l (fun _ -> em Empty)
                    | Empty -> [ em Empty ]
                    | _ -> List.empty<'t>
                |> EQ }

    | Shape.FSharpSet s ->
        s.Accept { new IFSharpSetVisitor<Mocker<'T>> with
            member __.Visit<'t when 't : comparison>() = // 'T = Set<'t>
                let em = mkMockerCached<'t> ctx
                let init n f = Seq.init n f |> Set.ofSeq
                fun mv ->
                    match mv with
                    | Static t -> t
                    | Variadic(:? 't as t) -> init 1 (fun _ -> t)
                    | VariadicCustomLength(:? 't as t, l) when l > 0 -> init l (fun _ -> t)
                    | CustomLength l when l > 0 -> init l (fun _ -> em Empty)
                    | Empty -> init 1 (fun _ -> em Empty)
                    | _ -> Set.empty<'t>
                |> EQ }

    | Shape.FSharpMap s ->
        s.Accept { new IFSharpMapVisitor<Mocker<'T>> with
            member __.Visit<'k, 'v when 'k : comparison>() = // 'T = Map<'k,'v>
                let km, vm = mkMockerCached<'k> ctx, mkMockerCached<'v> ctx
                let init n f = Seq.init n f |> Map.ofSeq
                fun mv ->
                    match mv with
                    | Static t -> t
                    | CustomLength l when l > 0 -> init l (fun _ -> km Empty, vm Empty)
                    | Empty -> init 1 (fun _ -> km Empty, vm Empty)
                    | _ -> Map.empty<'k,'v>
                |> EQ }

    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
        let elemMockers = shape.Elements |> Array.map mkMemberMocker
        fun _ ->
            let mutable inst = shape.CreateUninitialized()
            for e in elemMockers do inst <- e inst
            inst

    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
        let fieldMockers = shape.Fields |> Array.map mkMemberMocker
        fun _ ->
            let mutable inst = shape.CreateUninitialized()
            for f in fieldMockers do inst <- f inst
            inst

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
        let fieldMockerss = 
            shape.UnionCases 
            |> Array.map (fun c -> c, c.Fields |> Array.map mkMemberMocker)

        fun mv ->
            let tag =
                match mv with
                | PreferedUnionCase t -> t
                | _ -> Random().Next(0, fieldMockerss.Length)

            let shape, fieldMockers = fieldMockerss.[tag]
            let mutable inst = shape.CreateUninitialized()
            for fm in fieldMockers do inst <- fm inst
            inst

    | Shape.CliMutable (:? ShapeCliMutable<'T> as shape) ->
        let propMockers = shape.Properties |> Array.map mkMemberMocker
        fun _ ->
            let mutable inst = shape.CreateUninitialized()
            for f in propMockers do inst <- f inst
            inst

    | _ -> failwithf "Unsupported type '%O'" typeof<'T>

and private cache : TypeCache = new TypeCache()

type Mocker =
    static member Mock(?initSize : int) : 'T =
        let m = mkMocker<'T> ()
        let ctx = match initSize with None -> Empty | Some s -> CustomLength s
        m ctx


//-------------
// Examples

type Union =
    | A of int
    | B of string * int
    | C of byte[]

type Record =
    {
        [<Mock(Value = "Foo")>]
        A : string

        [<Mock(Value = 42, Size = 2)>]
        B : int list

        [<Mock(Size = 0)>]
        C : int option

        [<Mock(Value = 12, Size = 1)>]
        D : int option

        [<Mock(Value = true)>]
        F : bool

        [<Mock(PreferedUnionCase = 2)>]
        G : Union
    }

Mocker.Mock<Record list>()
//val it : Record list = [{A = "Foo";
//                         B = [42; 42];
//                         C = null;
//                         D = Some 12;
//                         F = true;
//                         G = C [|0uy|];}]