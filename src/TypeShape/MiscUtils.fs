#if TYPESHAPE_EXPOSE
module TypeShape_Utils
#else
module internal TypeShape_Utils
#endif

open System
open System.Threading
open System.Collections.Generic
open System.Collections.Concurrent

/// Value container that will eventually be populated
type Cell<'T> internal (container : 'T option ref) =
    let mutable isCreated = false
    let mutable value = Unchecked.defaultof<'T>
    member __.IsValueCreated : bool = 
        if isCreated then true else
        match !container with
        | None -> false
        | Some t -> value <- t ; isCreated <- true ; true

    member __.Value : 'T = 
        if isCreated then value else
        match !container with
        | None -> failwithf "Value for '%O' has not been initialized." typeof<'T>
        | Some t -> value <- t ; isCreated <- true ; t

[<NoEquality; NoComparison>]
type private RecTypePayload = { Cell : obj ; Value : obj ; IsValueSet : unit -> bool }

/// Helper class for generating recursive values
type RecTypeManager internal (parentCache : TypeCache option) = 
    let dict = new ConcurrentDictionary<Type, RecTypePayload>()

    new () = new RecTypeManager(None)
    member internal __.ParentCache = parentCache

    /// Attempt to look up value by type.
    /// If uninitialized rectype returns the placeholder dummy value.
    member __.TryGetValue<'T>(result : byref<'T>) : bool =
        let ok, payload = dict.TryGetValue typeof<'T>
        if ok then result <- payload.Value :?> 'T ; true
        else
            match parentCache with
            | None -> false
            | Some pc -> pc.TryGetValue<'T>(&result)

    /// Attempt to look up value by type.
    /// If uninitialized rectype returns the placeholder dummy value.
    member __.TryGetValue(t : Type, result : byref<obj>) : bool =
        let ok, payload = dict.TryGetValue t
        if ok then result <- payload.Value ; true
        else
            match parentCache with
            | None -> false
            | Some pc -> pc.TryGetValue(t, &result)

    /// Attempt to look up value by type.
    /// If uninitialized rectype returns the placeholder dummy value.
    member __.TryFind<'T>() =
        let ok, payload = dict.TryGetValue typeof<'T>
        if ok then Some(payload.Value :?> 'T)
        else
            match parentCache with
            | None -> None
            | Some pc -> pc.TryFind<'T>()

    /// Attempt to look up value by type.
    /// If uninitialized rectype returns the placeholder dummy value.
    member __.TryFind (t : Type) =
        let ok, payload = dict.TryGetValue t
        if ok then Some payload.Value
        else
            match parentCache with
            | None -> None
            | Some pc -> pc.TryFind t

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
            { Cell = container ; Value = dummy ; IsValueSet = fun () -> Option.isSome !container }

        let payload = dict.GetOrAdd(typeof<'T>, create)
        payload.Value :?> 'T

    /// Registers a value to the type index. Any uninitialized references 
    /// to this type will be updated to point to this value.
    member __.Complete<'T>(value : 'T) : 'T =
        let create _ =
            { Cell = ref (Some value) ; Value = value ; IsValueSet = fun () -> true }

        let update _ (payload : RecTypePayload) =
            if payload.IsValueSet() then payload 
            else
                lock payload.Cell (fun () ->
                    if payload.IsValueSet() then payload
                    else
                        payload.Cell :?> 'T option ref := Some value
                        { payload with Value = value })

        let payload = dict.AddOrUpdate(typeof<'T>, create, update)
        payload.Value :?> 'T

    member internal __.GetGeneratedValues() =
        let hasIncompleteValues = ref false
        let values =
            dict
            |> Seq.map (function 
                KeyValue(t, payload) ->
                    if not <| payload.IsValueSet() 
                    then hasIncompleteValues := true
                    (t, payload.Value))
            |> Seq.toArray

        if !hasIncompleteValues then [||] else values

    interface IDisposable with
        member __.Dispose() =
            match parentCache with
            | Some pc -> pc.Commit __
            | None -> ()

/// Thread-safe cache of values indexed by type.
and TypeCache internal (dict : ConcurrentDictionary<Type, obj>) =

    new () = TypeCache(new ConcurrentDictionary<_,_>())

    /// Total number of items in cache
    member __.Count = dict.Count
    /// Checks whether the supplied type is contained in cache
    member __.ContainsKey<'T>() = dict.ContainsKey typeof<'T>
    /// Checks whether the supplied type is contained in cache
    member __.ContainsKey(t : Type) = dict.ContainsKey t
    /// Gets all types registered in the cache
    member __.Keys = dict.Keys
    /// Gets all values registered in the cache
    member __.Values = dict.Values

    /// Try looking up cached value by type
    member __.TryGetValue<'T>(result : byref<'T>) : bool =
        let mutable obj = null
        if dict.TryGetValue(typeof<'T>, &obj) then
            result <- obj :?> 'T ; true
        else
            false

    /// Try looking up cached value by type
    member __.TryGetValue(t : Type, result : byref<obj>) : bool =
        let mutable obj = null
        if dict.TryGetValue(t, &obj) then
            result <- obj ; true
        else
            false

    /// Try looking up cached value by type
    member __.TryFind<'T>() : 'T option =
        let mutable obj = null
        if dict.TryGetValue(typeof<'T>, &obj) then Some(obj :?> 'T)
        else None

    /// Try looking up cached value by type
    member __.TryFind(t : Type) : obj option =
        let mutable obj = null
        if dict.TryGetValue(t, &obj) then Some obj
        else None

    /// Try adding value for given type
    member __.TryAdd<'T>(value : 'T) = dict.TryAdd(typeof<'T>, value)

    /// Forces update for value of given type
    member __.ForceAdd<'T>(value : 'T) = dict.[typeof<'T>] <- value

    /// Gets or adds value for given type using supplied factory.
    /// Uses optimistic concurrency
    member __.GetOrAdd<'T>(factory : unit -> 'T) : 'T =
        dict.GetOrAdd(typeof<'T>, fun _ -> factory() :> obj) :?> 'T

    /// Creates a RecTypeManager that is bound to the current cache.
    /// Values generated by the manager can be committed back to the
    /// cache once completed.
    member __.CreateRecTypeManager() = new RecTypeManager(Some __)

    /// Commits the generates state by a completed RecTypeManager instance.
    member __.Commit(manager : RecTypeManager) =
        match manager.ParentCache with
        | Some pc when pc = __ ->
            for k,v in manager.GetGeneratedValues() do 
                ignore(dict.TryAdd(k, v))

        | _ -> invalidArg "manager" "RecTypeManager does not belong to TypeCache context."

    /// Creates a clone of the current cache items
    member __.Clone() =
        let dict2 = new ConcurrentDictionary<Type, obj>(dict)
        new TypeCache(dict2)

/// Provides a binary search implementation for generic values
type BinSearch<'T when 'T : comparison>(inputs : 'T[]) =
    do 
        let duplicates =
            inputs 
            |> Seq.groupBy id
            |> Seq.filter(fun (_,gp) -> Seq.length gp > 1)
            |> Seq.map fst
            |> Seq.toArray

        if duplicates.Length > 0 then
            duplicates 
            |> Seq.map (sprintf "%A") 
            |> String.concat ","
            |> sprintf "duplicate values %s found"
            |> invalidArg "inputs"

    let indices, sortedInputs =
        inputs
        |> Seq.mapi (fun i v -> i,v)
        |> Seq.sortBy snd
        |> Seq.toArray
        |> Array.unzip

    /// Gets the original input array used to form
    /// this binary search implementation
    member __.Values = inputs

    /// Returns an integer indicating the position of the
    /// given value in the source array, or -1 if not found.
    member __.TryFindIndex(value : 'T) : int =
        match sortedInputs.Length with
        | 0 -> -1
        | 1 -> if sortedInputs.[0] = value then 0 else -1
        | n ->
            let mutable found = false
            let mutable lb, ub = 0, n - 1
            let mutable i = 0

            while not found && ub - lb >= 0 do
                i <- (lb + ub) / 2
                match compare value sortedInputs.[i] with
                | 0 -> found <- true
                | c when c < 0 -> ub <- i - 1
                | _ -> lb <- i + 1

            if found then indices.[i] else -1