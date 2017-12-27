module TypeShape.Core.SubtypeExtensions

open System
open System.IO
open TypeShape.Core

module Shape =

    /// Creates a subtype shape assuming that shape :> 'TBase
    let tryCreateSubtypeShape<'TBase> (shape : TypeShape) =
        if typeof<'TBase>.IsInterfaceAssignableFrom shape.Type then
            let inst = Activator.CreateInstanceGeneric<ShapeSubtype<_,_>> [|shape.Type ; typeof<'TBase>|]
            inst :?> IShapeSubtype<'TBase> |> Some
        else
            None

    /// Matches a shape whose type is a subtype of input shape
    let (|SubtypeOf|_|) (baseShape : TypeShape<'TBase>) (shape : TypeShape) = 
        ignore baseShape
        tryCreateSubtypeShape<'TBase> shape

    let (|IDisposable|_|) shape = tryCreateSubtypeShape<IDisposable> shape
    let (|Stream|_|) shape = tryCreateSubtypeShape<Stream> shape