#if TYPESHAPE_EXPOSE
module TypeShape_Utils
#else
module internal TypeShape_Utils
#endif

open System
open System.Collections.Generic

type private Cell<'T>() =
    let mutable value = Unchecked.defaultof<'T>
    let mutable isValueSet = false
    member __.IsValueSet = isValueSet
    member __.Value =
        if isValueSet then value
        else failwithf "Value for %O has not been initialized." typeof<'T>

    member __.SetValue(t:'T) = value <- t ; isValueSet <- true

type private Payload<'T> = Cell<'T> * 'T

/// State object for managing recursive types
type RecTypeManager() =
    let dict = new Dictionary<Type, obj>()

    /// Try to get value for spefic type
    member __.TryGetValue<'T>() =
        let ok, obj = dict.TryGetValue typeof<'T>
        if ok then
            let container, delayed = obj :?> Payload<'T>
            if container.IsValueSet then Some container.Value
            else Some delayed
        else
            None

    /// Creates a dummy value for given type using supplied delay function
    member __.Create<'T>(delay : (unit -> 'T) -> 'T) : 'T =
        if dict.ContainsKey typeof<'T> then
            failwithf "A definition for '%O' has already been created." typeof<'T>
        let container = new Cell<'T>()
        let delayed = delay (fun () -> container.Value)
        let payload : Payload<'T> = container, delayed
        dict.[typeof<'T>] <- box payload
        delayed

    /// Completes recursive definition for given type using supplied value
    member __.Complete<'T>(value : 'T) =
        let ok, obj = dict.TryGetValue typeof<'T>
        if ok then
            let container, _ = obj :?> Payload<'T>
            if container.IsValueSet then
                failwithf "A value for '%O' has already been set." typeof<'T>
            container.SetValue value
        else
            failwithf "No definition for '%O' has been created." typeof<'T>