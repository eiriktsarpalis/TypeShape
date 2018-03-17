module TypeShape.UnionEncoder

open System
open System.Collections.Generic
open System.Runtime.Serialization
open TypeShape.Core
open TypeShape.Core.Utils

/// Generic encoding abstraction for serializing/deserializing 
/// to a fixed format type, e.g. string, byte[] etc
type IEncoder<'Format> =
    abstract Encode : 'T -> 'Format
    abstract Decode : 'Format -> 'T

/// Generic encoder that simply upcasts values to System.Object
type CastEncoder() =
    interface IEncoder<obj> with
        member __.Encode t = box t
        member __.Decode o = unbox o

/// Represents an encoded event
type EncodedUnion<'Encoding> =
    {
        CaseName : string
        Payload  : KeyValuePair<string, 'Encoding> []
    }

/// Provides an encoder implementation for a sum of events
type IUnionEncoder<'Union, 'Format> =
    abstract GetCaseName   : 'Union -> string
    abstract Encode        : 'Union -> EncodedUnion<'Format>
    abstract Decode        : EncodedUnion<'Format> -> 'Union
    abstract TryDecode     : EncodedUnion<'Format> -> 'Union option

type private UnionCaseEncoder<'Union, 'Format> = 
    string * ('Union -> EncodedUnion<'Format>) * (EncodedUnion<'Format> -> 'Union)

type private UnionFieldEncoder<'Union, 'Format> =
    string * ('Union -> 'Format) * ('Union -> 'Format -> 'Union)

/// Generates an F# union encoder given a generic format encoder instance
let generateUnionEncoder<'Union, 'Format> (encoder : IEncoder<'Format>) =
    let shape =
        match shapeof<'Union> with
        | Shape.FSharpUnion (:? ShapeFSharpUnion<'Union> as s) -> s
        | _ -> 
            sprintf "Type '%O' is not an F# union" typeof<'Union>
            |> invalidArg "Union"

    let mkFieldEncoder (field : IShapeWriteMember<'Union>) =
        field.Accept {
            new IWriteMemberVisitor<'Union, UnionFieldEncoder<'Union, 'Format>> with
                member __.Visit(sfield : ShapeWriteMember<'Union, 'Field>) =
                    let enc u = sfield.Project u |> encoder.Encode
                    let dec u f = sfield.Inject u (encoder.Decode f)
                    sfield.Label, enc, dec
        }

    let genUnionCaseEncoder (scase : ShapeFSharpUnionCase<'Union>) =
        // extract the event type identifier for given case
        let eventType = 
            scase.CaseInfo.GetCustomAttributes() 
            |> Seq.tryPick (function :? DataMemberAttribute as dm -> Some dm.Name | _ -> None)
            |> (function None -> scase.CaseInfo.Name | Some v -> v)

        let fieldEncoders = scase.Fields |> Array.map mkFieldEncoder
        let labels = fieldEncoders |> Array.map (fun (l,_,_) -> l) |> BinSearch

        let enc (u:'Union) =
            let n = fieldEncoders.Length
            let r = Array.zeroCreate n
            for i = 0 to n - 1 do
                let k,fe,_ = fieldEncoders.[i]
                let e = fe u
                r.[i] <- KeyValuePair(k,e)

            { CaseName = eventType ; Payload = r }

        let dec (e:EncodedUnion<'Format>) =
            let mutable u = scase.CreateUninitialized()
            for kv in e.Payload do
                match labels.TryFindIndex kv.Key with
                | -1 -> ()
                |  i -> 
                    let _,_,fd = fieldEncoders.[i]
                    u <- fd u kv.Value
            u

        eventType, enc, dec

    let eventTypes, caseEncoders, caseDecoders = 
        shape.UnionCases 
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
        |> sprintf "Union type '%O' defines the following duplicate case identifiers: %s" typeof<'Union>
        |> invalidArg "Union"

    let unionCases = BinSearch eventTypes

    { new IUnionEncoder<'Union, 'Format> with
        member __.GetCaseName(u:'Union) =
            let tag = shape.GetTag u
            eventTypes.[tag]

        member __.Encode(u:'Union) =
            let tag = shape.GetTag u
            caseEncoders.[tag] u

        member __.Decode e =
            match unionCases.TryFindIndex e.CaseName with
            |  -1 ->
                let msg = sprintf "Unrecognized event type '%s'" e.CaseName
                raise <| FormatException msg
            | tag -> caseDecoders.[tag] e
                
        member __.TryDecode e =
            match unionCases.TryFindIndex e.CaseName with
            |  -1 -> None
            | tag -> caseDecoders.[tag] e |> Some
    }