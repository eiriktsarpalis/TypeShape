module JsonConverter

open System
open System.Runtime.CompilerServices
open System.Text.Json
open System.Text.Json.Serialization
open TypeShape.Core
open TypeShape.Core.Utils

module private Impl =

    type FieldWriter<'T> = delegate of Utf8JsonWriter * inref<'T> * JsonSerializerOptions -> unit
    type FieldReader<'T> = delegate of byref<Utf8JsonReader> * byref<'T> * JsonSerializerOptions -> unit

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let throwEOF() = raise <| JsonException "Unexpected end of JSON stream."

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let throwInvalidToken (expectedToken : JsonTokenType) (actualToken : JsonTokenType) = raise <| JsonException $"Expected JSON token type '{expectedToken}' but got '{actualToken}'."

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let throwMissingUnionCaseField (caseDiscriminatorPropertyName : string) = failwith $"F# union serializations must begin with a '{caseDiscriminatorPropertyName}' property."

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let throwUnrecognizedFSharpUnionCaseName (unionType : Type) (name : string) = raise <| JsonException $"Unrecognized case name {name} for F# union type {unionType}."

    let inline ensureRead (reader : byref<Utf8JsonReader>) =
        if not <| reader.Read() then throwEOF()

    let inline ensureToken (expectedToken : JsonTokenType) (reader : byref<Utf8JsonReader>) =
        if reader.TokenType <> expectedToken then throwInvalidToken expectedToken reader.TokenType

    let mkFieldSerializers (fieldShape : IShapeMember<'Tuple>) =
        fieldShape.Accept { new IMemberVisitor<'Tuple, FieldReader<'Tuple> * FieldWriter<'Tuple>> with
            member _.Visit<'Element> elementShape =
                let reader = FieldReader (fun reader target options -> 
                    let elementValue = JsonSerializer.Deserialize<'Element>(&reader, options)
                    elementShape.SetByRef(&target, elementValue))

                let writer = FieldWriter (fun writer source options ->
                    let elementValue = elementShape.GetByRef &source
                    JsonSerializer.Serialize(writer, elementValue, options))

                reader, writer
        }

    type JsonArrayTupleConverter<'Tuple>(shape : ShapeTuple<'Tuple>) =
        inherit JsonConverter<'Tuple>()

        let elementReaders, elementWriters = shape.Elements |> Array.map mkFieldSerializers |> Array.unzip

        override _.Write(writer, value, options) =
            writer.WriteStartArray()
            for eWriter in elementWriters do eWriter.Invoke(writer, &value, options)
            writer.WriteEndArray()

        override _.Read(reader, _, options) =
            ensureToken JsonTokenType.StartArray &reader

            let mutable tuple = shape.CreateUninitialized()
            for eReader in elementReaders do 
                ensureRead &reader
                eReader.Invoke(&reader, &tuple, options)

            ensureRead &reader
            ensureToken JsonTokenType.EndArray &reader
            tuple

    type FSharpUnionConverter<'Union>(shape : ShapeFSharpUnion<'Union>, caseDiscriminatorPropertyName : string, writeNullaryCasesAsStrings : bool) =
        inherit JsonConverter<'Union>()

        let cases =
            shape.UnionCases
            |> Array.map (fun caseShape ->
                {|
                    CaseShape = caseShape
                    CaseName = caseShape.CaseInfo.Name
                    FieldNameIndex = caseShape.Fields |> Seq.map (fun f -> f.Label) |> BinSearch
                    Fields = 
                        caseShape.Fields 
                        |> Array.map (fun f ->
                            let r, w = mkFieldSerializers f
                            {| Label = f.Label ; Reader = r ; Writer = w |})
                |})

        let caseNameIndex = cases |> Seq.map (fun c -> c.CaseName) |> BinSearch

        override _.Write(writer, value, options) =
            let tag = shape.GetTagByRef &value
            let case = cases.[tag]

            if writeNullaryCasesAsStrings && case.Fields.Length = 0 then
                writer.WriteStringValue case.CaseName
            else
                writer.WriteStartObject()
                writer.WriteString(caseDiscriminatorPropertyName, case.CaseName)
                for field in case.Fields do
                    writer.WritePropertyName field.Label
                    field.Writer.Invoke(writer, &value, options)

                writer.WriteEndObject()

        override _.Read(reader, _, options) =
            match reader.TokenType with
            | JsonTokenType.String when writeNullaryCasesAsStrings ->
                let caseName = reader.GetString()
                let tag = caseNameIndex.TryFindIndex caseName
                if tag < 0 then throwUnrecognizedFSharpUnionCaseName typeof<'Union> caseName
                cases.[tag].CaseShape.CreateUninitialized()

            | JsonTokenType.StartObject ->
                ensureRead &reader

                if reader.GetString() <> caseDiscriminatorPropertyName then
                    throwMissingUnionCaseField caseDiscriminatorPropertyName

                ensureRead &reader
                ensureToken JsonTokenType.String &reader
                let caseName = reader.GetString()
                let tag = caseNameIndex.TryFindIndex caseName
                if tag < 0 then throwUnrecognizedFSharpUnionCaseName typeof<'Union> caseName
                let case = cases.[tag]

                let mutable union = case.CaseShape.CreateUninitialized()
                while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
                    assert(reader.TokenType = JsonTokenType.PropertyName)
                    let fieldIndex = case.FieldNameIndex.TryFindIndex(reader.GetString())
                    ensureRead &reader
                    if fieldIndex < 0 then reader.Skip() else
                    case.Fields.[fieldIndex].Reader.Invoke(&reader, &union, options)

                union

            | token -> throwInvalidToken JsonTokenType.StartObject token

/// Serializes and deserializes tuples and struct tuples as heterogeneous JSON arrays.
type JsonArrayTupleConverter() =
    inherit JsonConverterFactory()

    override _.CanConvert t = match TypeShape.Create t with Shape.Tuple _ -> true | _ -> false
    override _.CreateConverter(t, _) =
        match TypeShape.Create t with
        | Shape.Tuple shape -> 
            shape.Accept {
                new ITupleVisitor<JsonConverter> with
                    member _.Visit<'Tuple> shape = new Impl.JsonArrayTupleConverter<'Tuple>(shape) :> JsonConverter
            }
        | _ -> invalidOp "Type not supported by converter factory."

/// Serializes and deserializes F# unions as JSON objects using case discriminator strings.
type FSharpUnionConverter(?caseDiscriminatorPropertyName : string, ?writeNullaryCasesAsStrings : bool) =
    inherit JsonConverterFactory()

    let caseDiscriminatorPropertyName = defaultArg caseDiscriminatorPropertyName "$case"
    let writeNullaryCasesAsStrings = defaultArg writeNullaryCasesAsStrings false

    override _.CanConvert t = 
        match TypeShape.Create t with 
        | Shape.FSharpOption _
        | Shape.FSharpList _ -> false
        | Shape.FSharpUnion _ -> true 
        | _ -> false

    override _.CreateConverter(t, _) =
        match TypeShape.Create t with
        | Shape.FSharpUnion shape -> 
            shape.Accept {
                new IFSharpUnionVisitor<JsonConverter> with
                    member _.Visit<'Union> shape =
                        new Impl.FSharpUnionConverter<'Union>(shape, caseDiscriminatorPropertyName, writeNullaryCasesAsStrings) :> JsonConverter
            }
        | _ -> invalidOp "Type not supported by converter factory."

//------------------------------------
// Examples

let options = new JsonSerializerOptions()
options.Converters.Add(new JsonArrayTupleConverter())
options.Converters.Add(new FSharpUnionConverter(writeNullaryCasesAsStrings = true))

let serialize<'T> (value : 'T) = JsonSerializer.Serialize(value, options)
let deserialize<'T> (json : string) = JsonSerializer.Deserialize<'T>(json, options)

serialize(42, "string", struct(false, true, true)) // [42,"string",[false,true,true]]
|> deserialize<int * string * struct(bool * bool * bool)>

type Enum = A | B | C

serialize [ A; B; C ] // ["A","B","C"]
|> deserialize<Enum list>

type Peano =
    | Z
    | S of succ:Peano

serialize [Z; S Z] // "["Z",{"$case":"S","succ":"Z"}]"
|> deserialize<Peano list>