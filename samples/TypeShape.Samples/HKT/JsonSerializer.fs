module JsonSerializer

open System
open System.Buffers
open System.Collections.Generic
open System.Text
open System.Text.Json
open System.Runtime.CompilerServices
open DotNext.Buffers
open TypeShape.HKT
open TypeShape.Core
open TypeShape.Core.Utils

/// Serializes and deserializes values of specified type
type JsonConverter<'T> =
    abstract Serialize : Utf8JsonWriter -> 'T -> unit
    abstract Deserialize : byref<Utf8JsonReader> -> 'T

/// Serializes and deserializes the existentially packed field of a given type
type JsonFieldConverter<'DeclaringType> =
    abstract PropertyName : string
    abstract SerializeField : Utf8JsonWriter * inref<'DeclaringType> -> unit
    abstract DeserializeField : byref<Utf8JsonReader> * byref<'DeclaringType> -> unit

[<AutoOpen>]
module private JsonHelpers =

    [<Literal>]
    let FSharpUnionCaseNameFieldName = "$case"

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let throwEOF() = failwith "Unexpected end of JSON stream."

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let throwInvalidToken (token : JsonTokenType) = failwith $"Unexpected JSON token type '{token}'."

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let throwMissingUnionCaseField () = failwith $"F# union serializations must begin with a '{FSharpUnionCaseNameFieldName}' property."

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let throwUnrecognizedFSharpUnionCaseName (unionType : Type) (name : string) = failwith $"Unrecognized case name {name} for F# union type {unionType}."

    let inline ensureRead (reader : byref<Utf8JsonReader>) =
        if not <| reader.Read() then throwEOF()

    let inline ensureToken (expectedToken : JsonTokenType) (reader : byref<Utf8JsonReader>) =
        if reader.TokenType <> expectedToken then throwInvalidToken reader.TokenType

// HKT encoding for JsonConverter
type JsonConverter =
    static member Assign(_ : App<JsonConverter, 'a>, _ : JsonConverter<'a>) = ()

/// HKT encoding for JsonFieldConverter
type JsonFieldConverter =
    static member Assign(_ : App<JsonFieldConverter, 'a>, _ : JsonFieldConverter<'a>) = ()

/// Tagless-final builder type for Json converters:
/// Can be inherited to extend support for more types
/// or to override existing implementations.
type JsonConverterBuilder() =
    interface IFSharpTypeBuilder<JsonConverter, JsonFieldConverter> with
        member _.Bool() =
            { new JsonConverter<bool> with
                member _.Serialize writer value = writer.WriteBooleanValue value
                member _.Deserialize reader = reader.GetBoolean() }
            |> HKT.pack

        member _.Byte() =
            { new JsonConverter<byte> with
                member _.Serialize writer value = writer.WriteNumberValue(uint value)
                member _.Deserialize reader = reader.GetByte() } 
            |> HKT.pack

        member _.SByte() =
            { new JsonConverter<sbyte> with
                member _.Serialize writer value = writer.WriteNumberValue(int value)
                member _.Deserialize reader = reader.GetSByte() } 
            |> HKT.pack

        member _.Char() =
            { new JsonConverter<char> with
                member _.Serialize writer value = writer.WriteStringValue(string value)
                member _.Deserialize reader = reader.GetString().[0] } 
            |> HKT.pack

        member _.Int16() =
            { new JsonConverter<int16> with
                member _.Serialize writer value = writer.WriteNumberValue(int value)
                member _.Deserialize reader = reader.GetInt16() } 
            |> HKT.pack

        member _.Int32() =
            { new JsonConverter<int32> with
                member _.Serialize writer value = writer.WriteNumberValue(value)
                member _.Deserialize reader = reader.GetInt32() } 
            |> HKT.pack

        member _.Int64() =
            { new JsonConverter<int64> with
                member _.Serialize writer value = writer.WriteNumberValue(value)
                member _.Deserialize reader = reader.GetInt64() } 
            |> HKT.pack

        member _.UInt16() =
            { new JsonConverter<uint16> with
                member _.Serialize writer value = writer.WriteNumberValue(int value)
                member _.Deserialize reader = reader.GetUInt16() } 
            |> HKT.pack

        member _.UInt32() =
            { new JsonConverter<uint32> with
                member _.Serialize writer value = writer.WriteNumberValue(value)
                member _.Deserialize reader = reader.GetUInt32() } 
            |> HKT.pack

        member _.UInt64() =
            { new JsonConverter<uint64> with
                member _.Serialize writer value = writer.WriteNumberValue(value)
                member _.Deserialize reader = reader.GetUInt64() } 
            |> HKT.pack

        member _.Single() =
            { new JsonConverter<single> with
                member _.Serialize writer value = writer.WriteNumberValue(value)
                member _.Deserialize reader = reader.GetSingle() } 
            |> HKT.pack

        member _.Double() =
            { new JsonConverter<double> with
                member _.Serialize writer value = writer.WriteNumberValue(value)
                member _.Deserialize reader = reader.GetDouble() } 
            |> HKT.pack

        member _.Decimal() =
            { new JsonConverter<decimal> with
                member _.Serialize writer value = writer.WriteNumberValue(value);
                member _.Deserialize reader = reader.GetDecimal() } 
            |> HKT.pack

        member _.BigInt() = raise <| NotSupportedException(nameof(bigint))

        member _.Unit() =
            // work around F# restriction on generic interfaces applied to unit
            let mkUnitConverter (_ : 'a) =
                { new JsonConverter<'a> with
                    // Unit is just the 0-ary tuple, so since tuples are serialized as JSON arrays
                    // we serialize unit as the empty array
                    member _.Serialize writer _ = writer.WriteStartArray(); writer.WriteEndArray()
                    member _.Deserialize reader = reader.Skip() ; Unchecked.defaultof<'a> } 

            mkUnitConverter () |> HKT.pack

        member _.String() =
            { new JsonConverter<string> with
                member _.Serialize writer value = writer.WriteStringValue value
                member _.Deserialize reader = reader.GetString() }
            |> HKT.pack

        member _.Guid() =
            { new JsonConverter<Guid> with
                member _.Serialize writer value = writer.WriteStringValue(value.ToString("D"))
                member _.Deserialize reader = reader.GetString() |> Guid }
            |> HKT.pack

        member _.TimeSpan() =
            { new JsonConverter<TimeSpan> with
                member _.Serialize writer value = writer.WriteStringValue(value.ToString("c"))
                member _.Deserialize reader = reader.GetString() |> TimeSpan.Parse }
            |> HKT.pack

        member _.DateTime() =
            { new JsonConverter<DateTime> with
                member _.Serialize writer value = writer.WriteStringValue(value.ToString("o"))
                member _.Deserialize reader = reader.GetString() |> DateTime.Parse }
            |> HKT.pack

        member _.DateTimeOffset() =
            { new JsonConverter<DateTimeOffset> with
                member _.Serialize writer value = writer.WriteStringValue(value.ToString("o"))
                member _.Deserialize reader = reader.GetString() |> DateTimeOffset.Parse }
            |> HKT.pack

        member _.Enum (HKT.Unpack underlyingConverter) =
            { new JsonConverter<'Enum> with 
                member _.Serialize writer value = underlyingConverter.Serialize writer (LanguagePrimitives.EnumToValue value)
                member _.Deserialize reader = underlyingConverter.Deserialize &reader |> LanguagePrimitives.EnumOfValue
            } |> HKT.pack

        member _.Nullable (HKT.Unpack elementConverter) =
            { new JsonConverter<Nullable<'t>> with 
                member _.Serialize writer value = 
                    if value.HasValue then elementConverter.Serialize writer value.Value 
                    else writer.WriteNullValue()
                member _.Deserialize reader =
                    match reader.TokenType with
                    | JsonTokenType.Null -> Nullable()
                    | _ -> Nullable(elementConverter.Deserialize &reader)
            } |> HKT.pack

        member _.Array (HKT.Unpack elementConverter) =
            { new JsonConverter<'t[]> with 
                member _.Serialize writer value =
                    writer.WriteStartArray()
                    for e in value do elementConverter.Serialize writer e
                    writer.WriteEndArray()

                member _.Deserialize reader =
                    ensureToken JsonTokenType.StartArray &reader
                    let array = ResizeArray<'t>()
                    while reader.Read() && reader.TokenType <> JsonTokenType.EndArray do
                        let e = elementConverter.Deserialize &reader
                        array.Add e

                    array.ToArray() }
            |> HKT.pack

        member _.Ref (HKT.Unpack elementConverter) =
            { new JsonConverter<'t ref> with
                member _.Serialize writer value = elementConverter.Serialize writer value.Value
                member _.Deserialize reader = elementConverter.Deserialize &reader |> ref }
            |> HKT.pack

        member _.Option (HKT.Unpack elementConverter) =
            { new JsonConverter<'t option> with
                member _.Serialize writer value =
                    match value with
                    | None -> writer.WriteNullValue()
                    | Some t -> elementConverter.Serialize writer t
                member _.Deserialize reader =
                    match reader.TokenType with
                    | JsonTokenType.Null -> None
                    | _ -> elementConverter.Deserialize &reader |> Some }
            |> HKT.pack

        member _.List (HKT.Unpack elementConverter) =
            { new JsonConverter<'t list> with 
                member _.Serialize writer value =
                    writer.WriteStartArray()
                    for e in value do elementConverter.Serialize writer e
                    writer.WriteEndArray()

                member _.Deserialize reader =
                    ensureToken JsonTokenType.StartArray &reader
                    let array = ResizeArray<'t>()
                    while reader.Read() && reader.TokenType <> JsonTokenType.EndArray do
                        let e = elementConverter.Deserialize &reader
                        array.Add e
                    List.ofSeq array }
            |> HKT.pack

        member _.Set (HKT.Unpack elementConverter) =
            { new JsonConverter<Set<'t>> with 
                member _.Serialize writer value =
                    writer.WriteStartArray()
                    for e in value do elementConverter.Serialize writer e
                    writer.WriteEndArray()

                member _.Deserialize reader =
                    ensureToken JsonTokenType.StartArray &reader
                    let array = new ResizeArray<'t>()
                    while reader.Read() && reader.TokenType <> JsonTokenType.EndArray do
                        let e = elementConverter.Deserialize &reader
                        array.Add e
                    Set.ofSeq array }
            |> HKT.pack

        member _.Map (HKT.Unpack (_ : JsonConverter<'key>)) (HKT.Unpack valueConverter) =
            if typeof<'key> <> typeof<string> then failwith $"Key type {typeof<'key>} is not supported." 

            { new JsonConverter<Map<string, 'v>> with 
                member _.Serialize writer value =
                    writer.WriteStartObject()
                    for kv in value do
                        writer.WritePropertyName kv.Key
                        valueConverter.Serialize writer kv.Value
                    writer.WriteEndObject()

                member _.Deserialize reader =
                    ensureToken JsonTokenType.StartObject &reader
                    let keyValuePairs = ResizeArray<string * 'v>()
                    while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
                        assert(reader.TokenType = JsonTokenType.PropertyName)
                        let key = reader.GetString()
                        ensureRead &reader
                        let value = valueConverter.Deserialize &reader
                        keyValuePairs.Add(key, value)  
                    Map.ofSeq keyValuePairs }
            |> unbox<JsonConverter<Map<'k, 'v>>> // 'k = string
            |> HKT.pack

        member _.Tuple shape (HKT.Unpacks fieldConverters) =
            // Serialize tuples as JSON arrays
            { new JsonConverter<'T> with
                member _.Serialize writer value =
                    writer.WriteStartArray()
                    for fieldConverter in fieldConverters do fieldConverter.SerializeField (writer, &value)
                    writer.WriteEndArray()

                member _.Deserialize reader =
                    ensureToken JsonTokenType.StartArray &reader
                    let mutable result = shape.CreateUninitialized()
                    for fieldConverter in fieldConverters do
                        ensureRead &reader
                        fieldConverter.DeserializeField(&reader, &result)
                    ensureRead &reader
                    ensureToken JsonTokenType.EndArray &reader
                    result }
            |> HKT.pack

        member _.Record shape (HKT.Unpacks fieldConverters) =
            let labelSearch = fieldConverters |> Seq.map (fun c -> c.PropertyName) |> BinSearch
            { new JsonConverter<'T> with
                member _.Serialize writer value =
                    writer.WriteStartObject()
                    for fieldConverter in fieldConverters do 
                        writer.WritePropertyName fieldConverter.PropertyName
                        fieldConverter.SerializeField (writer, &value)
                    writer.WriteEndObject()

                member _.Deserialize reader =
                    ensureToken JsonTokenType.StartObject &reader
                    let mutable result = shape.CreateUninitialized()
                    while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
                        assert(reader.TokenType = JsonTokenType.PropertyName)
                        let fieldIndex = labelSearch.TryFindIndex(reader.GetString())
                        ensureRead &reader
                        if fieldIndex < 0 then reader.Skip() else
                        let fp = fieldConverters.[fieldIndex]
                        fp.DeserializeField(&reader, &result)

                    result }
            |> HKT.pack

        member _.Union shape (HKT.Unpackss fieldConverterss) =
            let caseNameSearch = shape.UnionCases |> Seq.map (fun c -> c.CaseInfo.Name) |> BinSearch
            let unionCases = 
                (shape.UnionCases, fieldConverterss)
                ||> Array.map2 (fun unionShape fieldConverters  ->
                    let labelSearch = unionShape.Fields |> Array.map (fun f -> f.Label) |> BinSearch
                    (labelSearch, unionShape, fieldConverters))

            { new JsonConverter<'T> with
                member _.Serialize writer value =
                    let tag = shape.GetTagByRef &value
                    let _, caseInfo, fieldConverters = unionCases.[tag]
                    if fieldConverters.Length = 0 then
                        writer.WriteStringValue caseInfo.CaseInfo.Name
                    else
                        writer.WriteStartObject()
                        writer.WritePropertyName JsonHelpers.FSharpUnionCaseNameFieldName
                        writer.WriteStringValue caseInfo.CaseInfo.Name
                        for fieldConverter in fieldConverters do 
                            writer.WritePropertyName fieldConverter.PropertyName
                            fieldConverter.SerializeField (writer, &value)
                        writer.WriteEndObject()

                member _.Deserialize reader =
                    match reader.TokenType with
                    | JsonTokenType.String ->
                        let caseName = reader.GetString()
                        let tag = caseNameSearch.TryFindIndex caseName
                        if tag < 0 then JsonHelpers.throwUnrecognizedFSharpUnionCaseName typeof<'T> caseName
                        let _,caseInfo,_ = unionCases.[tag]
                        caseInfo.CreateUninitialized()
                    | JsonTokenType.StartObject ->
                        ensureRead &reader
                        assert(reader.TokenType = JsonTokenType.PropertyName)
                        if reader.GetString() <> JsonHelpers.FSharpUnionCaseNameFieldName then
                            JsonHelpers.throwMissingUnionCaseField()

                        ensureRead &reader
                        ensureToken JsonTokenType.String &reader
                        let caseName = reader.GetString()
                        let tag = caseNameSearch.TryFindIndex caseName
                        if tag < 0 then JsonHelpers.throwUnrecognizedFSharpUnionCaseName typeof<'T> caseName
                        let labelSearch, caseInfo, fieldConverters = unionCases.[tag]

                        let mutable result = caseInfo.CreateUninitialized()
                        while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
                            assert(reader.TokenType = JsonTokenType.PropertyName)
                            let fieldIndex = labelSearch.TryFindIndex(reader.GetString())
                            ensureRead &reader
                            if fieldIndex < 0 then reader.Skip() else
                            let fieldConverter = fieldConverters.[fieldIndex]
                            fieldConverter.DeserializeField(&reader, &result)
                        result
                    | token -> JsonHelpers.throwInvalidToken token }
            |> HKT.pack

    interface ICliMutableBuilder<JsonConverter, JsonFieldConverter> with
        member _.CliMutable shape (HKT.Unpacks fieldConverters) =
            let labelSearch = fieldConverters |> Seq.map (fun c -> c.PropertyName) |> BinSearch
            { new JsonConverter<'T> with
                member _.Serialize writer value =
                    writer.WriteStartObject()
                    for fp in fieldConverters do 
                        writer.WritePropertyName fp.PropertyName
                        fp.SerializeField(writer, &value)
                    writer.WriteEndObject()

                member _.Deserialize reader =
                    ensureToken JsonTokenType.StartObject &reader
                    let mutable result = shape.CreateUninitialized()
                    while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
                        assert(reader.TokenType = JsonTokenType.PropertyName)
                        let fieldIndex = labelSearch.TryFindIndex(reader.GetString())
                        ensureRead &reader
                        if fieldIndex < 0 then reader.Skip() else
                        let fp = fieldConverters.[fieldIndex]
                        fp.DeserializeField(&reader, &result)

                    result }
            |> HKT.pack

    interface IFieldExtractor<JsonConverter, JsonFieldConverter> with
        member _.Field shape (HKT.Unpack fieldConverter) = 
            { new JsonFieldConverter<'DeclaringType> with
                member _.PropertyName = shape.Label
                member _.SerializeField (writer, value) =
                    let field = shape.GetByRef &value
                    fieldConverter.Serialize writer field

                member _.DeserializeField (reader, value) =
                    let field = fieldConverter.Deserialize &reader
                    shape.SetByRef(&value, field) }
            |> HKT.pack

/// Tagless-final converter folding logic
type ConverterGenerator<'JsonConverterBuilder 
                            when 'JsonConverterBuilder :> JsonConverterBuilder
                            and 'JsonConverterBuilder : (new : unit -> 'JsonConverterBuilder)>() =

    let cache = Some (new TypeCache())

    member val Builder = new 'JsonConverterBuilder()
    member this.GenerateConverter<'t> () : JsonConverter<'t> = FoldContext.fold cache this |> HKT.unpack

    abstract Fold<'t> : self:IGenericProgram<JsonConverter> -> App<JsonConverter, 't>
    default this.Fold<'t> self =
        match shapeof<'t> with
        | Fold.FSharpType this.Builder self s -> s
        | Fold.CliMutable this.Builder self s -> s
        | _ -> failwith $"Serialization for type {typeof<'t>} is not supported."

    interface IFoldContext<JsonConverter> with 
        member this.Fold<'t> self = this.Fold<'t> self
        member this.Delay converterRef =
            let c = lazy(HKT.unpack converterRef.Value)
            { new JsonConverter<'T> with
                member _.Serialize writer value = c.Value.Serialize writer value
                member _.Deserialize reader = c.Value.Deserialize &reader }
            |> HKT.pack

//-----------------------------------
// Root-level serialization functions

let serialize (pickler : JsonConverter<'T>) (value : 'T) : string =
    use bufferWriter = new PooledArrayBufferWriter<byte>()
    do
        use writer = new Utf8JsonWriter(bufferWriter)
        pickler.Serialize writer value
    Encoding.UTF8.GetString bufferWriter.WrittenMemory.Span

let deserialize (pickler : JsonConverter<'T>) (json : string) : 'T =
    let maxSize = Encoding.UTF8.GetMaxByteCount json.Length
    let buffer = ArrayPool<byte>.Shared.Rent(maxSize)
    try
        let size = Encoding.UTF8.GetBytes(json.AsSpan(), Span buffer);
        let mutable reader = new Utf8JsonReader(ReadOnlySpan(buffer).Slice(0, size))
        JsonHelpers.ensureRead &reader
        pickler.Deserialize &reader
    finally
        ArrayPool<byte>.Shared.Return(buffer)

//--------------------------
// Examples    

let private generator = new ConverterGenerator<JsonConverterBuilder>()
let generateConverter<'t>() = generator.GenerateConverter<'t>()

let p0 = generateConverter<int>()

serialize p0 42 // "42"
|> deserialize p0

let p1 = generateConverter<int * int list>()

serialize p1 (1, [2;3;4]) // "[1,[2,3,4]]"
|> deserialize p1

let p2 = generateConverter<int * string list option * string ref>()

serialize p2 (1, Some(["a";"b";"c"]), ref "foo") // "[1,["a","b","c"],"foo"]"
|> deserialize p2

type Bar =
    | Foo of values: {| A : int ; B : string |}
    | Bar of number:int
    | C
    | D of item:string option
    | E of items:Map<string, int>

let p3 = generateConverter<Bar list []>()

// "[[],["C",{"$case":"D","item":null}],[{"$case":"Foo","values":{"A":42,"B":"12"}}],[{"$case":"E","items":{"foo":42}}]]"
serialize p3 [| [] ; [C ; D None] ; [Foo {| A = 42 ; B = "12" |}] ; [E (Map.ofList [("foo", 42)])]|]
|> deserialize p3

// Recursive type serialization
type BinTree<'T> = Leaf | Node of value:'T * left:BinTree<'T> * right:BinTree<'T>

let p4 = generateConverter<BinTree<int>> ()

// "{"$case":"Node","value":3,"left":{"$case":"Node","value":1,"left":"Leaf","right":{"$case":"Node","value":2,"left":"Leaf","right":"Leaf"}},"right":"Leaf"}"
serialize p4 (Node(3, Node(1, Leaf, Node(2, Leaf,Leaf)), Leaf))
|> deserialize p4


//--------------------------
// Extensibility

type ConverterBuilderExtensions() =
    inherit JsonConverterBuilder()

    // override BigInt serialization
    interface IFSharpTypeBuilder<JsonConverter, JsonFieldConverter> with
        member _.BigInt() =
            { new JsonConverter<bigint> with
                member _.Serialize writer value = writer.WriteStringValue(value.ToString());
                member _.Deserialize reader = reader.GetString() |> bigint.Parse } 
            |> HKT.pack

    // add support for dictionary converters
    interface IDictionaryBuilder<JsonConverter> with
        member _.Dictionary (HKT.Unpack (_ : JsonConverter<'key>)) (HKT.Unpack valueConverter) =
            if typeof<'key> <> typeof<string> then failwith $"Key type {typeof<'key>} is not supported." 

            { new JsonConverter<Dictionary<string, 'value>> with
                member _.Serialize writer dict =
                    writer.WriteStartObject()
                    for kv in dict do
                        writer.WritePropertyName kv.Key
                        valueConverter.Serialize writer kv.Value
                    writer.WriteEndObject()

                member _.Deserialize reader =
                    ensureToken JsonTokenType.StartObject &reader
                    let dict = new Dictionary<string, 'value>()
                    while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
                        assert(reader.TokenType = JsonTokenType.PropertyName)
                        let key = reader.GetString()
                        ensureRead &reader
                        let value = valueConverter.Deserialize &reader
                        dict.Add(key, value)  
                    dict }
            |> unbox<JsonConverter<Dictionary<'key, 'value>>> // 'key = string
            |> HKT.pack

type ConverterGeneratorExtensions() =
    inherit ConverterGenerator<ConverterBuilderExtensions>()
    override this.Fold<'t> self =
        match shapeof<'t> with
        | Fold.Dictionary this.Builder self s -> s
        | _ -> base.Fold<'t> self

let generatorExtensions = new ConverterGeneratorExtensions()
let dictConverter = generatorExtensions.GenerateConverter<Dictionary<string, bigint>>()

serialize dictConverter (let dict = new Dictionary<string, bigint>() in dict.Add("42", 42I) ; dict)
|> deserialize dictConverter