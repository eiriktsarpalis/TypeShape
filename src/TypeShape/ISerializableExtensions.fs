#if TYPESHAPE_HIDE
module internal TypeShape_ISerializableExtensions
#else
module TypeShape_ISerializableExtensions
#endif

#nowarn "4224"

open TypeShape
open System
open System.Runtime.Serialization

type ISerializableVisitor<'R> =
    abstract Visit<'T when 'T :> ISerializable> : unit -> 'R

type IShapeISerializable =
    abstract Accept : ISerializableVisitor<'R> -> 'R

type private ShapeISerializable<'T when 'T :> ISerializable> () =
    inherit TypeShape<'T>()
    interface IShapeISerializable with
        member __.Accept v = v.Visit<'T>()

type ShapeSerializable =
    static member Resolver : TypeShapeResolver =
        fun (t : Type) ->
            if typeof<exn>.IsAssignableFrom t then None
            elif typeof<ISerializable>.IsInterfaceAssignableFrom t then
                let shape = Activator.CreateInstanceGeneric(typedefof<ShapeISerializable<_>>, [|t|]) :?> TypeShape
                Some shape
            else
                None

[<RequireQualifiedAccess>]
module Shape =
    let (|ISerializable|_|) t = Shape.test<IShapeISerializable> t