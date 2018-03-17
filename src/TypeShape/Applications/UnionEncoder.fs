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

[<AutoOpen>]
module private Impl =

    type UnionFieldEncoder<'Union, 'Format> =
        string * ('Union -> 'Format) * ('Union -> 'Format -> 'Union)

    type EncoderGenerator<'Union, 'Format> =
        // requireRecord -> minArity -> maxArity -> ..
        bool -> int option -> int option -> IEncoder<'Format> -> IUnionEncoder<'Union, 'Format>

    /// Generates an F# union encoder given a generic format encoder instance
    let mkUnionEncoder<'Union, 'Format> () : EncoderGenerator<'Union, 'Format> =
        let shape =
            match shapeof<'Union> with
            | Shape.FSharpUnion (:? ShapeFSharpUnion<'Union> as s) -> s
            | _ -> 
                sprintf "Type '%O' is not an F# union" typeof<'Union>
                |> invalidArg "Union"

        let mkFieldEncDec (encoder : IEncoder<'Format>) (field : IShapeWriteMember<'Union>) =
            field.Accept {
                new IWriteMemberVisitor<'Union, UnionFieldEncoder<'Union, 'Format>> with
                    member __.Visit(sfield : ShapeWriteMember<'Union, 'Field>) =
                        let enc u = sfield.Project u |> encoder.Encode
                        let dec u f = sfield.Inject u (encoder.Decode f)
                        sfield.Label, enc, dec
            }

        let hasRecordPayload (scase : ShapeFSharpUnionCase<'Union>) =
            match scase.Fields with
            | [||] -> true
            | [| field |] ->
                match field.Member with
                | Shape.FSharpRecord _ -> true
                | _ -> false
            | _ -> false

        let genUnionCaseEncoder (scase : ShapeFSharpUnionCase<'Union>) =
            // extract the case label identifier for given case
            let caseLabel =
                scase.CaseInfo.GetCustomAttributes()
                |> Seq.tryPick (function :? DataMemberAttribute as dm -> Some dm.Name | _ -> None)
                |> (function None -> scase.CaseInfo.Name | Some v -> v)

            let labels = 
                scase.Fields 
                |> Array.map (fun f -> f.Label) 
                |> BinSearch

            let mkCaseEncDec encoder =
                let fieldEncoders = scase.Fields |> Array.map (mkFieldEncDec encoder)

                let enc (u:'Union) =
                    let n = fieldEncoders.Length
                    let r = Array.zeroCreate n
                    for i = 0 to n - 1 do
                        let k,fe,_ = fieldEncoders.[i]
                        let e = fe u
                        r.[i] <- KeyValuePair(k,e)

                    { CaseName = caseLabel ; Payload = r }

                let dec (e:EncodedUnion<'Format>) =
                    let mutable u = scase.CreateUninitialized()
                    for kv in e.Payload do
                        match labels.TryFindIndex kv.Key with
                        | -1 -> ()
                        |  i -> 
                            let _,_,fd = fieldEncoders.[i]
                            u <- fd u kv.Value
                    u

                enc, dec

            caseLabel, mkCaseEncDec

        let eventTypes, caseEncoders = 
            shape.UnionCases 
            |> Array.map genUnionCaseEncoder
            |> Array.unzip

        let caseWithoutRecordPayload =
            shape.UnionCases
            |> Array.tryFind (not << hasRecordPayload)

        let minArityCase, maxArityCase =
            let getArity (scase : ShapeFSharpUnionCase<'Union>) = scase.Arity
            shape.UnionCases |> Array.minBy getArity,
            shape.UnionCases |> Array.maxBy getArity

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

        fun requireRecordPayloads minAllowedArity maxAllowedArity encoder ->
            if requireRecordPayloads && Option.isSome caseWithoutRecordPayload then
                sprintf "Union case '%O.%O' contains field which is not an F# record" typeof<'Union> caseWithoutRecordPayload.Value
                |> invalidArg "Union"

            match minAllowedArity, maxAllowedArity with
            | Some min, _ when minArityCase.Arity < min ->
                sprintf "Union case %O.%O has arity %d which is less than the minimum %d"
                    typeof<'Union> minArityCase.CaseInfo.Name minArityCase.Arity min
                |> invalidArg "Union"
            | _, Some max when maxArityCase.Arity > max ->
                sprintf "Union case %O.%O has arity %d which is less than the minimum %d"
                    typeof<'Union> minArityCase.CaseInfo.Name minArityCase.Arity max
                |> invalidArg "Union"
            | _ -> ()

            let caseEncoders,caseDecoders =
                caseEncoders
                |> Array.map (fun e -> e encoder)
                |> Array.unzip

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

    type EncoderFactory<'Union, 'Format> private () =
        static let factory = lazy(mkUnionEncoder<'Union, 'Format>())
        static member Create reqRec min max (e : IEncoder<_>) = factory.Value reqRec min max e


type UnionEncoder<'Union> =
    /// <summary>
    ///     Given a primite object encoder instance, generates
    ///     an F# union encoder which constructs and deconstructs
    ///     DU instances into a flat structure.
    /// </summary>
    static member Create(encoder : IEncoder<'Format>, ?minArity : int, ?maxArity : int, ?requireRecordPayload : bool) : IUnionEncoder<'Union, 'Format> = 
        EncoderFactory<'Union, 'Format>.Create (defaultArg requireRecordPayload false) minArity maxArity encoder