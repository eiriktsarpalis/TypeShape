#r "../bin/Release/net40/TypeShape.dll"
open System
open TypeShape.Core
open TypeShape.Core.Utils

// Generic map implementation that updates all occurences
// of a given type inside a value

type GMapper<'E, 'T> = ('E -> 'E) -> ('T -> 'T)

let rec gmap<'E, 'T> (mapper : 'E -> 'E) : 'T -> 'T =
    match cache.TryFind<GMapper<'E, 'T>> () with
    | Some m -> m mapper
    | None ->
        use ctx = cache.CreateRecTypeManager()
        gmapCached<'E, 'T> ctx mapper

and private gmapCached<'E, 'T> (ctx : RecTypeManager) : GMapper<'E, 'T> =
    match ctx.TryFind<GMapper<'E, 'T>> () with
    | Some m -> m
    | None ->
        let _ = ctx.CreateUninitialized<GMapper<'E, 'T>> (fun c f -> c.Value f)
        let m = gmapAux<'E, 'T> ctx
        ctx.Complete m

and private gmapAux<'E, 'T> (ctx : RecTypeManager) : GMapper<'E, 'T> =
    let EQ (input : GMapper<'E, 'a>) : GMapper<'E, 'T> = unbox input

    let gmapMember (shape : IShapeWriteMember<'Class>) =
        shape.Accept { new IWriteMemberVisitor<'Class, ('E -> 'E) -> 'Class -> 'Class -> 'Class> with
            member __.Visit (shape : ShapeWriteMember<'Class, 'Field>) =
                let fMapper = gmapCached<'E, 'Field> ctx
                fun mapper source target ->
                    let field = shape.Project source
                    let field' = fMapper mapper field
                    shape.Inject target field'
        }

    match shapeof<'T> with
    | :? TypeShape<'E> -> EQ id<'E -> 'E>
    | Shape.Primitive
    | Shape.String
    | Shape.Guid
    | Shape.Decimal
    | Shape.DateTime
    | Shape.DateTimeOffset -> EQ (fun _ -> id<'T>)
    | Shape.FSharpOption s ->
        s.Accept { new IFSharpOptionVisitor<GMapper<'E, 'T>> with
            member __.Visit<'t> () = // 't option = 'T
                let em = gmapCached<'E, 't> ctx
                EQ(fun f -> Option.map (em f))
        }

    | Shape.Array s when s.Rank = 1 ->
        s.Accept { new IArrayVisitor<GMapper<'E, 'T>> with
            member __.Visit<'t> _ = // 't [] = 'T
                let em = gmapCached<'E, 't> ctx
                EQ(fun f -> Array.map (em f))
        }

    | Shape.FSharpList s ->
        s.Accept { new IFSharpListVisitor<GMapper<'E, 'T>> with
            member __.Visit<'t> () = // 't list = 'T
                let em = gmapCached<'E, 't> ctx
                EQ(fun f -> List.map (em f))
        }

    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
        let ems = shape.Elements |> Array.map gmapMember
        fun mapper source ->
            let mutable target = shape.CreateUninitialized()
            for em in ems do target <- em mapper source target
            target

    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
        let fms = shape.Fields |> Array.map gmapMember
        fun mapper source ->
            let mutable target = shape.CreateUninitialized()
            for fm in fms do target <- fm mapper source target
            target

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
        let caseMappers = 
            shape.UnionCases 
            |> Array.map (fun case -> case, case.Fields |> Array.map gmapMember)

        fun mapper source ->
            let tag = shape.GetTag source
            let case, mappers = caseMappers.[tag]
            let mutable target = case.CreateUninitialized()
            for fm in mappers do target <- fm mapper source target
            target
        
    | _ -> failwithf "Unsupported type '%O'" typeof<'T>

and private cache : TypeCache = new TypeCache()


//-------------------------------
// Examples

gmap ((+) 1) (Some [| [1 .. 10] |], 1, ("foo", 3, (5,Some 6)))

type Person = { Name : string ; Age : int ; Address : string }

let value =
    [ { Name = "george" ; Age = 31 ; Address = "Dublin" } ;
      { Name = "john" ; Age = 40; Address = "8th Avenue" } ;
      { Name = "Paul" ; Age = 74; Address = "England" } ]

gmap (fun (s:string) -> s.ToUpper()) value