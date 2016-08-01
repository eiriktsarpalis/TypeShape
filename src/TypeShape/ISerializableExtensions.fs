#if TYPESHAPE_HIDE
module internal TypeShape_ISerializableExtensions
#else
module TypeShape_ISerializableExtensions
#endif

open TypeShape
open System
open System.Runtime.Serialization

type ISerializableVisitor<'R> =
    abstract Visit<'T when 'T :> ISerializable> : unit -> 'R

type IShapeISerializable =
    abstract Accept : ISerializableVisitor<'R> -> 'R

type private ShapeISerializable<'T when 'T :> ISerializable> () =
    interface IShapeISerializable with
        member __.Accept v = v.Visit<'T>()

[<RequireQualifiedAccess>]
module Shape =
    let (|ISerializable|_|) (shape : TypeShape) =
        if typeof<ISerializable>.IsInterfaceAssignableFrom shape.Type then
            Activator.CreateInstanceGeneric<ShapeISerializable<_>>([|shape.Type|])
            :?> IShapeISerializable
            |> Some
        else
            None