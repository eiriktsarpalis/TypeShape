module TypeShape.UnionContract

open System
open System.Runtime.Serialization
open TypeShape.Core
open TypeShape.Core.Utils

/// Marker interface for tagging union contract types
/// see the <a href="https://eiriktsarpalis.wordpress.com/2018/10/30/a-contract-pattern-for-schemaless-datastores">A Contract Pattern for Schemaless DataStores</a> blog article for an overview
type IUnionContract = interface end

/// Generic encoding abstraction for serializing/deserializing 
/// to a fixed format type, e.g. string, byte[], Newtonsoft JsonValue etc
type IEncoder<'Format> =
    /// 'Null' format value to be used for union cases without a payload
    abstract Empty : 'Format
    /// Encodes any value to the implementation format
    abstract Encode<'T> : value:'T -> 'Format
    /// Decodes format instance to any value
    abstract Decode<'T> : fmt:'Format -> 'T

/// Generic encoder that simply upcasts values to System.Object
type BoxEncoder() =
    interface IEncoder<obj> with
        member __.Empty    = null
        member __.Encode t = box t
        member __.Decode o = unbox o

/// Represents an encoded union case
type EncodedUnion<'Encoding> =
    {
        CaseName : string
        Payload  : 'Encoding
    }

/// Provides an encoder implementation for a sum of events
[<AbstractClass>]
type UnionContractEncoder<'Union, 'Format> internal () =
    /// Gets the union case string identifier for given union instance
    abstract GetCaseName : value:'Union -> string
    /// Encodes a union instance into a decoded representation
    abstract Encode      : value:'Union -> EncodedUnion<'Format>
    /// Decodes a formatted representation into a union instance. Can potentially throw FormatException
    abstract Decode      : encodedUnion:EncodedUnion<'Format> -> 'Union
    /// Decodes a formatted representation into a union instance. Does not throw exception on format mismatches
    abstract TryDecode   : encodedUnion:EncodedUnion<'Format> -> 'Union option

[<AutoOpen>]
module private Impl =

    type Config = { requireRecordPayloads : bool ; allowNullaryCases : bool }

    type EncoderGenerator<'Union, 'Format> = Config -> IEncoder<'Format> -> UnionContractEncoder<'Union, 'Format>

    /// Generates an F# union encoder given a generic format encoder instance
    let mkUnionEncoder<'Union, 'Format> () : EncoderGenerator<'Union, 'Format> =
        let shape =
            match shapeof<'Union> with
            | Shape.FSharpUnion (:? ShapeFSharpUnion<'Union> as s) -> s
            | _ -> 
                sprintf "Type '%O' is not an F# union" typeof<'Union>
                |> invalidArg "Union"

        let mkFieldEncoders (field : IShapeMember<'Union>) (encoder : IEncoder<'Format>) =
            field.Accept {
                new IMemberVisitor<'Union, ('Union -> 'Format) * ('Union -> 'Format -> 'Union)> with
                    member __.Visit(sfield : ShapeMember<'Union, 'Field>) =
                        let enc u = 
                            let f = sfield.Get u 
                            encoder.Encode f

                        let dec u f =
                            let v = encoder.Decode f
                            sfield.Set u v

                        enc, dec
            }

        let hasRecordPayload (scase : ShapeFSharpUnionCase<'Union>) =
            match scase.Fields with
            | [| |] -> true // support as trivial case
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

            let caseField =
                match scase.Fields with
                | [| |] -> None
                | [| field |] -> Some field
                | _ ->
                    sprintf "union case %O.%O has more than one fields." typeof<'Union> scase.CaseInfo.Name
                    |> invalidArg "Union"

            let mkCaseEncDec encoder =
                match caseField with
                | Some f ->
                    let fieldEnc, fieldDec = mkFieldEncoders f encoder
                    let enc u = { CaseName = label ; Payload = fieldEnc u }
                    let dec f =
                        let u = scase.CreateUninitialized()
                        fieldDec u f

                    enc, dec

                | None -> 
                    let enc _ = { CaseName = label ; Payload = encoder.Empty }
                    let dec _ = scase.CreateUninitialized()
                    enc, dec

            label, mkCaseEncDec

        let labels, encoderFactories = 
            shape.UnionCases 
            |> Array.map genUnionCaseEncoder
            |> Array.unzip

        let caseWithoutRecordPayload =
            shape.UnionCases
            |> Array.tryFind (not << hasRecordPayload)

        let nullaryUnionCase =
            shape.UnionCases
            |> Array.tryFind (fun c -> Array.isEmpty c.Fields)

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

        fun config encoder ->
            match caseWithoutRecordPayload with
            | Some case when config.requireRecordPayloads ->
                sprintf "Union case '%O.%O' contains field that is not an F# record" typeof<'Union> case.CaseInfo.Name
                |> invalidArg "Union"
            | _ -> ()

            match nullaryUnionCase with
            | Some case when not config.allowNullaryCases ->
                sprintf "Union case '%O.%O' is nullary" typeof<'Union> case.CaseInfo.Name
                |> invalidArg "Union"
            | _ -> ()

            let caseEncoders,caseDecoders =
                encoderFactories
                |> Array.map (fun e -> e encoder)
                |> Array.unzip

            { new UnionContractEncoder<'Union, 'Format>() with
                member __.GetCaseName(u:'Union) =
                    let tag = shape.GetTag u
                    labels.[tag]

                member __.Encode(u:'Union) =
                    let tag = shape.GetTag u
                    caseEncoders.[tag] u

                member __.Decode e =
                    match labelIndex.TryFindIndex e.CaseName with
                    |  -1 ->
                        let msg = sprintf "Unrecognized case name '%O.%s'" typeof<'Union> e.CaseName
                        raise <| FormatException msg
                    | tag -> caseDecoders.[tag] e.Payload
                
                member __.TryDecode e =
                    match labelIndex.TryFindIndex e.CaseName with
                    |  -1 -> None
                    | tag -> caseDecoders.[tag] e.Payload |> Some }

    type EncoderFactory<'Union, 'Format> private () =
        static let factory = lazy(mkUnionEncoder<'Union, 'Format>())
        static member Create requireRecordFields (e : IEncoder<_>) = factory.Value requireRecordFields e


type UnionContractEncoder =

    /// <summary>
    ///     Given a primite object encoder instance, generates
    ///     an F# union encoder which constructs and deconstructs
    ///     DU instances into a flat structure. All union cases must
    ///     contain exactly one field.
    /// </summary>
    /// <param name="encoder">Encoder used for converting union case payloads into given format.</param>
    /// <param name="requireRecordFields">Fail encoder generation if union cases contain fields that are not F# records. Defaults to false.</param>
    /// <param name="allowNullaryCases">Fail encoder generation if union contains nullary cases. Defaults to true.</param>
    static member Create<'Union, 'Format when 'Union :> IUnionContract>(encoder : IEncoder<'Format>, ?requireRecordFields : bool, ?allowNullaryCases : bool) : UnionContractEncoder<'Union, 'Format> = 
        let config = { requireRecordPayloads = defaultArg requireRecordFields false ; allowNullaryCases = defaultArg allowNullaryCases true }
        EncoderFactory<'Union, 'Format>.Create config encoder