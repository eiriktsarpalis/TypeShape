#if TYPESHAPE_EXPOSE
module TypeShape_Utils
#else
module internal TypeShape_Utils
#endif

open System
open System.Collections.Concurrent

/// Value container that will eventually be populated
type Cell<'T> internal (container : 'T option ref) =
    member __.IsValueCreated = Option.isSome !container
    member __.Value = 
        match container.Value with
        | None -> failwithf "Value for '%O' has not been initialized." typeof<'T>
        | Some t -> t

type private Payload<'T> = 'T option ref * 'T

/// Thread-safe cache of values indexed by type that supports recursion.
type TypedIndex() =
    let dict = new ConcurrentDictionary<Type, obj>()

    /// <summary>
    ///     Registers an uninitialized value at the beggining of a recursive
    ///     value definition. Returns a dummy value that can be referenced within a
    ///     recursive flow. Only delayable values can be recursive.
    /// </summary>
    /// <param name="delay">Provides delay wrapping for supplied type.</param>
    member __.CreateUninitialized<'T>(delay : Cell<'T> -> 'T) : 'T =
        let create _ =
            let container = ref None
            let dummy = delay (Cell container)
            let payload : Payload<'T> = container, dummy
            box payload

        let _, value = dict.GetOrAdd(typeof<'T>, create) :?> Payload<'T>
        value

    /// Registers a value to the type index. Any uninitialized references 
    /// to this type will be updated to point to this value.
    member __.Commit<'T>(value : 'T) : 'T =
        let create _ =
            let payload : Payload<'T> = ref (Some value), value
            box payload

        let update _ (currentValue : obj) =
            let container, _ = currentValue :?> Payload<'T>
            lock container (fun () ->
                match container.Value with
                | Some _ -> currentValue
                | None ->
                    container := Some value
                    let newPayload : Payload<'T> = container, value
                    box newPayload)

        let _,value = dict.AddOrUpdate(typeof<'T>, create, update) :?> Payload<'T>
        value

    member __.TryGetValue<'T>(result : byref<'T>) : bool =
        let mutable obj = null
        if dict.TryGetValue(typeof<'T>, &obj) then
            let _,value = obj :?> Payload<'T>
            result <- value ; true
        else
            false

    member __.TryFind<'T>() =
        let mutable t = Unchecked.defaultof<_>
        if __.TryGetValue<'T>(&t) then Some t
        else None