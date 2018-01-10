module TypeShape.EventSum

open System
open System.Runtime.Serialization
open FSharp.Reflection
open TypeShape.Core
open TypeShape.Core.Utils

/// Generic encoding abstraction for serializing/deserializing 
/// to a fixed format type, e.g. string, byte[] etc
type IEncoder<'Format> =
    /// Empty or Nil representation for given format
    abstract Empty  : 'Format
    abstract Encode : 'T -> 'Format
    abstract Decode : 'Format -> 'T

type CastEncoder() =
    interface IEncoder<obj> with
        member __.Empty = null
        member __.Encode t = box t
        member __.Decode o = unbox o

/// Represents an encoded event
type EncodedEvent<'Encoding> =
    {
        EventType : string
        Payload : 'Encoding
    }

/// Provides an encoder implementation for a sum of events
type IEventSumEncoder<'Union, 'Format> =
    abstract Encode : 'Union -> EncodedEvent<'Format>
    abstract Decode : EncodedEvent<'Format> -> 'Union
    abstract TryDecode : EncodedEvent<'Format> -> 'Union option

type private UnionCaseEncoder<'Union, 'Format> = 
    string * ('Union -> EncodedEvent<'Format>) * (EncodedEvent<'Format> -> 'Union)

/// Generates an event sum encoder given an event encoder instance
/// Union cases should all contain at most one F# record as a field
let generateSumEventEncoder<'Union, 'Format> (encoder : IEncoder<'Format>) =
    match shapeof<'Union> with
    | Shape.FSharpUnion (:? ShapeFSharpUnion<'Union> as sunion) ->
        let genUnionCaseEncoder (scase : ShapeFSharpUnionCase<'Union>) =
            // extract the event type identifier for given case
            let eventType = 
                scase.CaseInfo.GetCustomAttributes() 
                |> Seq.tryPick (function :? DataMemberAttribute as dm -> Some dm.Name | _ -> None)
                |> (function None -> scase.CaseInfo.Name | Some v -> v)

            match scase.Fields with
            | [||] ->
                let enc (_ : 'Union) = { EventType = eventType ; Payload = encoder.Empty }
                let dec (_ : EncodedEvent<'Format>) = scase.CreateUninitialized()
                eventType, enc, dec

            | [|field|] ->
                field.Accept { 
                    new IWriteMemberVisitor<'Union, UnionCaseEncoder<'Union, 'Format>> with
                      member __.Visit(sfield : ShapeWriteMember<'Union, 'Field>) =
                        match shapeof<'Field> with
                        | Shape.FSharpRecord _ ->

                            let enc u = { EventType = eventType ; Payload = encoder.Encode(sfield.Project u) }
                            let dec (e:EncodedEvent<'Format>) = 
                                let u = scase.CreateUninitialized()
                                let r = encoder.Decode<'Record> e.Payload
                                sfield.Inject u r

                            eventType, enc, dec

                        | _ ->
                            sprintf "Union case '%O' contains field of type '%O' which is not an F# record" scase.CaseInfo.Name field.Member
                            |> invalidArg "Union"
                }

            | _ ->
                sprintf "Union case '%O' can contain at most one field which should be an F# record" scase.CaseInfo.Name
                |> invalidArg "Union"

        let eventTypes, caseEncoders, caseDecoders = 
            sunion.UnionCases 
            |> Array.map genUnionCaseEncoder
            |> Array.unzip3

        // check for duplicate union case labels
        let duplicates =
            eventTypes 
            |> Seq.groupBy id
            |> Seq.filter (fun (_,items) -> Seq.length items > 1)
            |> Seq.map fst
            |> Seq.toArray

        if duplicates.Length > 0 then
            String.concat ", " duplicates
            |> sprintf "Union case '%O' defines the following duplicate case identifiers: %s" typeof<'Union>
            |> invalidArg "Union"

        let unionCases = BinSearch eventTypes

        { new IEventSumEncoder<'Union, 'Format> with
            member __.Encode(u:'Union) =
                let tag = sunion.GetTag u
                caseEncoders.[tag] u

            member __.Decode e =
                match unionCases.TryFindIndex e.EventType with
                | -1 ->
                    let msg = sprintf "Unrecognized event type '%s'" e.EventType
                    raise <| FormatException msg
                | tag -> caseDecoders.[tag] e
                
            member __.TryDecode e =
                match unionCases.TryFindIndex e.EventType with
                | -1 -> None
                | tag -> caseDecoders.[tag] e |> Some
        }

    | _ -> 
        sprintf "Type '%O' is not an F# union" typeof<'Union>
        |> invalidArg "Union"