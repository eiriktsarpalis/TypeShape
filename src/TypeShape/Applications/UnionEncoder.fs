module TypeShape.UnionEncoder

open System
open System.Runtime.Serialization
open TypeShape.Core
open TypeShape.Core.Utils

/// Generic encoding abstraction for serializing/deserializing 
/// to a fixed format type, e.g. string, byte[], JToken etc
type IEncoder<'Format> =
    abstract Encode  : 'T -> 'Format
    abstract Decode  : 'Format -> 'T

/// Generic encoder that simply upcasts values to System.Object
type CastEncoder() =
    interface IEncoder<obj> with
        member __.Encode t = box t
        member __.Decode o = unbox o

/// Represents an encoded event
type EncodedUnion<'Encoding> =
    {
        Label    : string
        Payload  : 'Encoding
    }

/// Provides an encoder implementation for a sum of events
type IUnionEncoder<'Union, 'Format> =
    abstract GetLabel      : 'Union -> string
    abstract Encode        : 'Union -> EncodedUnion<'Format>
    abstract Decode        : EncodedUnion<'Format> -> 'Union
    abstract TryDecode     : EncodedUnion<'Format> -> 'Union option

[<AutoOpen>]
module private Impl =

    type EncoderGenerator<'Union, 'Format> = 
        (* requireRecordPayloads:*)bool -> IEncoder<'Format> -> IUnionEncoder<'Union, 'Format>

    /// Generates an F# union encoder given a generic format encoder instance
    let mkUnionEncoder<'Union, 'Format> () : EncoderGenerator<'Union, 'Format> =
        let shape =
            match shapeof<'Union> with
            | Shape.FSharpUnion (:? ShapeFSharpUnion<'Union> as s) -> s
            | _ -> 
                sprintf "Type '%O' is not an F# union" typeof<'Union>
                |> invalidArg "Union"

        let hasRecordPayload (scase : ShapeFSharpUnionCase<'Union>) =
            match scase.Fields with
            | [| field |] ->
                match field.Member with
                | Shape.FSharpRecord _ -> true
                | _ -> false
            | _ -> false

        let genUnionCaseEncoder (scase : ShapeFSharpUnionCase<'Union>) =
            // extract the case label identifier for given case
            let label =
                scase.CaseInfo.GetCustomAttributes()
                |> Seq.tryPick (function :? DataMemberAttribute as dm -> Some dm.Name | _ -> None)
                |> (function None -> scase.CaseInfo.Name | Some v -> v)

            let field =
                match scase.Fields with
                | [| field |] -> field
                | _ ->
                    sprintf "union case %O.%O has arity <> 1" typeof<'Union> scase.CaseInfo.Name
                    |> invalidArg "Union"

            let mkEncoders (encoder : IEncoder<'Format>) =
                field.Accept {
                    new IWriteMemberVisitor<'Union, ('Union -> 'Format) * ('Format -> 'Union)> with
                        member __.Visit(sfield : ShapeWriteMember<'Union, 'Field>) =
                            let enc u = sfield.Project u |> encoder.Encode
                            let dec f = 
                                let v = encoder.Decode f
                                let u = scase.CreateUninitialized()
                                sfield.Inject u v

                            enc, dec
                }

            label, mkEncoders

        let labels, encoderFactories = 
            shape.UnionCases 
            |> Array.map genUnionCaseEncoder
            |> Array.unzip

        let caseWithoutRecordPayload =
            shape.UnionCases
            |> Array.tryFind (not << hasRecordPayload)

        // check for duplicate union case labels
        let duplicates =
            labels
            |> Seq.groupBy id
            |> Seq.filter (fun (_,items) -> Seq.length items > 1)
            |> Seq.map fst
            |> Seq.toArray

        if duplicates.Length > 0 then
            String.concat ", " duplicates
            |> sprintf "Union type '%O' defines the following duplicate case identifiers: %s" typeof<'Union>
            |> invalidArg "Union"

        let labelIndex = BinSearch labels

        fun requireRecordPayloads encoder ->
            if requireRecordPayloads && Option.isSome caseWithoutRecordPayload then
                sprintf "Union case '%O.%O' contains field which is not an F# record" typeof<'Union> caseWithoutRecordPayload.Value
                |> invalidArg "Union"

            let caseEncoders,caseDecoders =
                encoderFactories
                |> Array.map (fun e -> e encoder)
                |> Array.unzip

            { new IUnionEncoder<'Union, 'Format> with
                member __.GetLabel(u:'Union) =
                    let tag = shape.GetTag u
                    labels.[tag]

                member __.Encode(u:'Union) =
                    let tag = shape.GetTag u
                    { Label = labels.[tag] ; Payload = caseEncoders.[tag] u }

                member __.Decode e =
                    match labelIndex.TryFindIndex e.Label with
                    |  -1 ->
                        let msg = sprintf "Unrecognized event type '%s'" e.Label
                        raise <| FormatException msg
                    | tag -> caseDecoders.[tag] e.Payload
                
                member __.TryDecode e =
                    match labelIndex.TryFindIndex e.Label with
                    |  -1 -> None
                    | tag -> caseDecoders.[tag] e.Payload |> Some
            }

    type EncoderFactory<'Union, 'Format> private () =
        static let factory = lazy(mkUnionEncoder<'Union, 'Format>())
        static member Create requireRecordFields (e : IEncoder<_>) = factory.Value requireRecordFields e


type UnionEncoder<'Union> =
    /// <summary>
    ///     Given a primite object encoder instance, generates
    ///     an F# union encoder which constructs and deconstructs
    ///     DU instances into a flat structure. All union cases must
    ///     contain exactly one field.
    /// </summary>
    /// <param name="encoder">Encoder used for converting union case payloads into given format.</param>
    /// <param name="requireRecordFields">Fail encoder generation if union cases contain fields that are not F# records. Defaults to false.</param>
    static member Create(encoder : IEncoder<'Format>, ?requireRecordFields : bool) : IUnionEncoder<'Union, 'Format> = 
        EncoderFactory<'Union, 'Format>.Create (defaultArg requireRecordFields false) encoder