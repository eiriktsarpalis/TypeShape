module TypeShape_SubtypeExtensions

open System

open TypeShape

module Shape =
    open System.IO

    /// Creates a subtype shape assuming that shape :> 'TBase
    let tryCreateSubtypeShape<'TBase> (shape : TypeShape) =
        if typeof<'TBase>.IsInterfaceAssignableFrom shape.Type then
            let inst = Activator.CreateInstanceGeneric<ShapeSubtype<_,_>> [|shape.Type ; typeof<'TBase>|]
            inst :?> IShapeSubtype<'TBase> |> Some
        else
            None

    let (|IDisposable|_|) shape = tryCreateSubtypeShape<IDisposable> shape
    let (|Stream|_|) shape = tryCreateSubtypeShape<Stream> shape