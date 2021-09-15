module CborSerializer

open System
open System.Buffers
open System.Formats.Cbor
open System.Runtime.CompilerServices
open TypeShape.HKT
open TypeShape.Core
open TypeShape.Core.Utils
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic

#nowarn "9"

// Serializes and deserializes values of specified type
type CborConverter<'T> =
    abstract Serialize : CborWriter -> 'T -> unit
    abstract Deserialize : CborReader -> 'T

/// Serializes and deserializes the existentially packed field of a given type
type CborFieldConverter<'DeclaringType> =
    abstract PropertyName : string
    abstract SerializeField : CborWriter * inref<'DeclaringType> -> unit
    abstract DeserializeField : CborReader * byref<'DeclaringType> -> unit

[<AutoOpen>]
module private CborHelpers =

    [<Literal>]
    let FSharpUnionCaseNameFieldName = "$case"

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let throwInvalidToken (state : CborReaderState) = failwith $"Unexpected CBOR token type '{state}'."

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let throwInvalidTag (tag : CborTag) = failwith $"Unexpected CBOR tag '{tag}'."

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let throwMissingUnionCaseField () = failwith $"F# union serializations must begin with a '{FSharpUnionCaseNameFieldName}' property."

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let throwUnrecognizedFSharpUnionCaseName (unionType : Type) (name : string) = failwith $"Unrecognized case name {name} for F# union type {unionType}."
    
    module CborTag =
        let inline mkTag value : CborTag = LanguagePrimitives.EnumOfValue value

        // https://www.iana.org/assignments/cbor-tags/cbor-tags.xhtml
        let UUID = mkTag 37UL
        let durationSeconds = mkTag 1002UL

    let inline ensureTag expectedTag (reader : CborReader) =
        if reader.ReadTag() <> expectedTag then throwInvalidTag expectedTag

    let inline stackAllocSpan<'T when 'T : unmanaged> (size) =
        let mem = NativePtr.stackalloc<'T>(size) |> NativePtr.toVoidPtr
        Span<'T>(mem, size)

    let bytesToHex (source : ReadOnlySpan<byte>) : string =
        let charBuffer = ArrayPool<char>.Shared.Rent (2 * source.Length)
        let mutable i = 0
        for byte in source do
            let inline nibbleToChar (nibble : byte) =
                if nibble < 10uy then char nibble + '0'
                else char nibble + '7' // '7' = 'A' - 10

            charBuffer.[i] <- nibbleToChar(byte >>> 4)
            charBuffer.[i + 1] <- nibbleToChar(byte &&& 0x0Fuy)
            i <- i + 2

        let str = new String(ReadOnlySpan(charBuffer, 0, i))
        ArrayPool<char>.Shared.Return charBuffer
        str

    let hexToBytes (source : ReadOnlySpan<char>) (target : Span<byte>) =
        assert(source.Length <= 2 * target.Length)
        let mutable i = 0
        while i < source.Length do
            target.[i >>> 1] <- Byte.Parse(source.Slice(i, 2), Globalization.NumberStyles.HexNumber)
            i <- i + 2

// HKT encoding for CborConverter
type CborConverter =
    static member Assign(_ : App<CborConverter, 'a>, _ : CborConverter<'a>) = ()

/// HKT encoding for CborFieldConverter
type CborFieldConverter =
    static member Assign(_ : App<CborFieldConverter, 'a>, _ : CborFieldConverter<'a>) = ()

/// Tagless-final builder type for Cbor converters:
/// Can be inherited to extend support for more types
/// or to override existing implementations.
type CborConverterBuilder() =
    interface IByteArrayBuilder<CborConverter> with
        member _.ByteArray() =
            { new CborConverter<byte[]> with
                member _.Serialize writer value = writer.WriteByteString value
                member _.Deserialize reader = reader.ReadByteString() }
            |> HKT.pack

    interface IFSharpTypeBuilder<CborConverter, CborFieldConverter> with
        member _.Bool() =
            { new CborConverter<bool> with
                member _.Serialize writer value = writer.WriteBoolean value
                member _.Deserialize reader = reader.ReadBoolean() }
            |> HKT.pack

        member _.Byte() =
            { new CborConverter<byte> with
                member _.Serialize writer value = writer.WriteUInt32(uint value)
                member _.Deserialize reader = reader.ReadUInt32() |> byte } 
            |> HKT.pack

        member _.SByte() =
            { new CborConverter<sbyte> with
                member _.Serialize writer value = writer.WriteInt32(int value)
                member _.Deserialize reader = reader.ReadInt32() |> sbyte } 
            |> HKT.pack

        member _.Char() =
            { new CborConverter<char> with
                member _.Serialize writer value = writer.WriteTextString(string value)
                member _.Deserialize reader = reader.ReadTextString().[0] } 
            |> HKT.pack

        member _.Int16() =
            { new CborConverter<int16> with
                member _.Serialize writer value = writer.WriteInt32(int value)
                member _.Deserialize reader = reader.ReadInt32() |> int16 } 
            |> HKT.pack

        member _.Int32() =
            { new CborConverter<int32> with
                member _.Serialize writer value = writer.WriteInt32(value)
                member _.Deserialize reader = reader.ReadInt32() } 
            |> HKT.pack

        member _.Int64() =
            { new CborConverter<int64> with
                member _.Serialize writer value = writer.WriteInt64(value)
                member _.Deserialize reader = reader.ReadInt64() } 
            |> HKT.pack

        member _.UInt16() =
            { new CborConverter<uint16> with
                member _.Serialize writer value = writer.WriteUInt32(uint value)
                member _.Deserialize reader = reader.ReadUInt32() |> uint16 } 
            |> HKT.pack

        member _.UInt32() =
            { new CborConverter<uint32> with
                member _.Serialize writer value = writer.WriteUInt32(value)
                member _.Deserialize reader = reader.ReadUInt32() } 
            |> HKT.pack

        member _.UInt64() =
            { new CborConverter<uint64> with
                member _.Serialize writer value = writer.WriteUInt64(value)
                member _.Deserialize reader = reader.ReadUInt64() } 
            |> HKT.pack

        member _.Single() =
            { new CborConverter<single> with
                member _.Serialize writer value = writer.WriteSingle(value)
                member _.Deserialize reader = reader.ReadSingle() } 
            |> HKT.pack

        member _.Double() =
            { new CborConverter<double> with
                member _.Serialize writer value = writer.WriteDouble(value)
                member _.Deserialize reader = reader.ReadDouble() } 
            |> HKT.pack

        member _.Decimal() =
            { new CborConverter<decimal> with
                member _.Serialize writer value = writer.WriteDecimal(value);
                member _.Deserialize reader = reader.ReadDecimal() } 
            |> HKT.pack

        member _.BigInt() =
            { new CborConverter<bigint> with
                member _.Serialize writer value = writer.WriteBigInteger(value);
                member _.Deserialize reader = reader.ReadBigInteger() } 
            |> HKT.pack

        member _.Unit() =
            // work around F# restriction on generic interfaces applied to unit
            let mkUnitConverter (_ : 'a) =
                { new CborConverter<'a> with
                    // Unit is just the 0-ary tuple, so since tuples are serialized as CBOR arrays
                    // we serialize unit as the empty array
                    member _.Serialize writer _ = writer.WriteStartArray(0) ; writer.WriteEndArray()
                    member _.Deserialize reader = reader.SkipValue() ; Unchecked.defaultof<'a> } 

            mkUnitConverter () |> HKT.pack

        member _.String() =
            { new CborConverter<string> with
                member _.Serialize writer value = match value with null -> writer.WriteNull() | _ -> writer.WriteTextString value
                member _.Deserialize reader = 
                    match reader.PeekState() with
                    | CborReaderState.Null -> reader.SkipValue() ; null
                    | _ -> reader.ReadTextString() }
            |> HKT.pack

        member _.Guid() =
            { new CborConverter<Guid> with
                member _.Serialize writer value =
                    let buffer = stackAllocSpan<byte> 16
                    let result = value.TryWriteBytes(buffer)
                    assert(result)
                    writer.WriteTag CborTag.UUID
                    writer.WriteByteString(Span.op_Implicit buffer)

                member _.Deserialize reader =
                    ensureTag CborTag.UUID reader
                    let data = reader.ReadDefiniteLengthByteString()
                    Guid data.Span }
            |> HKT.pack

        member _.TimeSpan() =
            { new CborConverter<TimeSpan> with
                member _.Serialize writer value = writer.WriteTag CborTag.durationSeconds ; writer.WriteDouble(value.TotalSeconds)
                member _.Deserialize reader = 
                    ensureTag CborTag.durationSeconds reader
                    match reader.PeekState() with
                    | CborReaderState.UnsignedInteger
                    | CborReaderState.NegativeInteger -> TimeSpan.FromSeconds(double(reader.ReadInt64())) 
                    | CborReaderState.HalfPrecisionFloat
                    | CborReaderState.SinglePrecisionFloat
                    | CborReaderState.DoublePrecisionFloat -> TimeSpan.FromSeconds(reader.ReadDouble())
                    | token -> throwInvalidToken token }
            |> HKT.pack

        member _.DateTime() =
            { new CborConverter<DateTime> with
                member _.Serialize writer value = writer.WriteDateTimeOffset(DateTimeOffset value)
                member _.Deserialize reader = reader.ReadDateTimeOffset().DateTime }
            |> HKT.pack

        member _.DateTimeOffset() =
            { new CborConverter<DateTimeOffset> with
                member _.Serialize writer value = writer.WriteDateTimeOffset value
                member _.Deserialize reader = reader.ReadDateTimeOffset() }
            |> HKT.pack

        member _.Enum (HKT.Unpack underlyingConverter) =
            { new CborConverter<'Enum> with 
                member _.Serialize writer value = underlyingConverter.Serialize writer (LanguagePrimitives.EnumToValue value)
                member _.Deserialize reader = underlyingConverter.Deserialize reader |> LanguagePrimitives.EnumOfValue
            } |> HKT.pack

        member _.Nullable (HKT.Unpack elementConverter) =
            { new CborConverter<Nullable<'t>> with 
                member _.Serialize writer value = 
                    if value.HasValue then elementConverter.Serialize writer value.Value 
                    else writer.WriteNull()
                member _.Deserialize reader =
                    match reader.PeekState() with
                    | CborReaderState.Null -> reader.SkipValue() ; Nullable()
                    | _ -> Nullable(elementConverter.Deserialize reader)
            } |> HKT.pack

        member _.Array (HKT.Unpack elementConverter) =
            { new CborConverter<'t[]> with
                member _.Serialize writer value =
                    writer.WriteStartArray value.Length
                    for e in value do elementConverter.Serialize writer e
                    writer.WriteEndArray()

                member _.Deserialize reader =
                    let length = reader.ReadStartArray()
                    if length.HasValue then
                        let array = Array.zeroCreate<'t> length.Value
                        for i = 0 to length.Value - 1 do
                            array.[i] <- elementConverter.Deserialize reader
                        reader.ReadEndArray()
                        array
                    else
                        let array = ResizeArray<'t>()
                        while reader.PeekState() <> CborReaderState.EndArray do
                            array.Add (elementConverter.Deserialize reader)
                        reader.ReadEndArray()
                        array.ToArray() }
            |> HKT.pack

        member _.Ref (HKT.Unpack elementConverter) =
            { new CborConverter<'t ref> with
                member _.Serialize writer value = elementConverter.Serialize writer value.Value
                member _.Deserialize reader = elementConverter.Deserialize reader |> ref }
            |> HKT.pack

        member _.Option (HKT.Unpack elementConverter) =
            { new CborConverter<'t option> with
                member _.Serialize writer value =
                    match value with
                    | None -> writer.WriteNull()
                    | Some t -> elementConverter.Serialize writer t
                member _.Deserialize reader =
                    match reader.PeekState() with
                    | CborReaderState.Null -> reader.SkipValue() ; None
                    | _ -> elementConverter.Deserialize reader |> Some }
            |> HKT.pack

        member _.List (HKT.Unpack elementConverter) =
            { new CborConverter<'t list> with 
                member _.Serialize writer value =
                    writer.WriteStartArray value.Length
                    for e in value do elementConverter.Serialize writer e
                    writer.WriteEndArray()

                member _.Deserialize reader =
                    let length = reader.ReadStartArray()
                    if length.HasValue then
                        let array = Array.zeroCreate<'t> length.Value
                        for i = 0 to length.Value - 1 do
                            array.[i] <- elementConverter.Deserialize reader
                        reader.ReadEndArray()
                        List.ofArray array
                    else
                        let array = ResizeArray<'t>()
                        while reader.PeekState() <> CborReaderState.EndArray do
                            array.Add (elementConverter.Deserialize reader)
                        reader.ReadEndArray()
                        List.ofSeq array }
            |> HKT.pack

        member _.Set (HKT.Unpack elementConverter) =
            { new CborConverter<Set<'t>> with 
                member _.Serialize writer value =
                    writer.WriteStartArray value.Count
                    for e in value do elementConverter.Serialize writer e
                    writer.WriteEndArray()

                member _.Deserialize reader =
                    let length = reader.ReadStartArray()
                    if length.HasValue then
                        let array = Array.zeroCreate<'t> length.Value
                        for i = 0 to length.Value - 1 do
                            array.[i] <- elementConverter.Deserialize reader
                        reader.ReadEndArray()
                        Set.ofArray array
                    else
                        let array = ResizeArray<'t>()
                        while reader.PeekState() <> CborReaderState.EndArray do
                            array.Add (elementConverter.Deserialize reader)
                        reader.ReadEndArray()
                        Set.ofSeq array }
            |> HKT.pack

        member _.Map (HKT.Unpack keyConverter) (HKT.Unpack valueConverter) =
            { new CborConverter<Map<'key, 'value>> with 
                member _.Serialize writer value =
                    writer.WriteStartMap value.Count
                    for kv in value do
                        keyConverter.Serialize writer kv.Key
                        valueConverter.Serialize writer kv.Value
                    writer.WriteEndMap()

                member _.Deserialize reader =
                    let length = reader.ReadStartMap()
                    if length.HasValue then
                        let array = Array.zeroCreate<'key * 'value> length.Value
                        for i = 0 to length.Value - 1 do
                            array.[i] <- (keyConverter.Deserialize reader, valueConverter.Deserialize reader)
                        reader.ReadEndMap()
                        Map.ofArray array
                    else
                        let array = ResizeArray<'key * 'value>()
                        while reader.PeekState() <> CborReaderState.EndMap do
                            array.Add (keyConverter.Deserialize reader, valueConverter.Deserialize reader)
                        reader.ReadEndMap()
                        Map.ofSeq array }
            |> HKT.pack

        member _.Tuple shape (HKT.Unpacks fieldConverters) =
            // Serialize tuples as CBOR arrays
            { new CborConverter<'T> with
                member _.Serialize writer value =
                    writer.WriteStartArray shape.Elements.Length
                    for fieldConverter in fieldConverters do fieldConverter.SerializeField(writer, &value)
                    writer.WriteEndArray()

                member _.Deserialize reader =
                    let _ = reader.ReadStartArray()
                    let mutable result = shape.CreateUninitialized()
                    for fieldConverter in fieldConverters do fieldConverter.DeserializeField(reader, &result)
                    reader.ReadEndArray()
                    result }
            |> HKT.pack

        member _.Record shape (HKT.Unpacks fieldConverters) =
            let labelSearch = fieldConverters |> Seq.map (fun c -> c.PropertyName) |> BinSearch
            { new CborConverter<'T> with
                member _.Serialize writer value =
                    writer.WriteStartMap shape.Fields.Length
                    for fieldConverter in fieldConverters do 
                        writer.WriteTextString fieldConverter.PropertyName
                        fieldConverter.SerializeField(writer, &value)
                    writer.WriteEndMap()

                member _.Deserialize reader =
                    let _ = reader.ReadStartMap()
                    let mutable result = shape.CreateUninitialized()
                    while reader.PeekState() <> CborReaderState.EndMap do
                        let key = reader.ReadTextString() // TODO spanify key reads
                        let fieldIndex = labelSearch.TryFindIndex key
                        if fieldIndex < 0 then reader.SkipValue() else
                        let fp = fieldConverters.[fieldIndex]
                        fp.DeserializeField(reader, &result)
                    reader.ReadEndMap()
                    result }
            |> HKT.pack

        member _.Union shape (HKT.Unpackss fieldConverterss) =
            let caseNameSearch = shape.UnionCases |> Seq.map (fun c -> c.CaseInfo.Name) |> BinSearch
            let unionCases = 
                (shape.UnionCases, fieldConverterss)
                ||> Array.map2 (fun unionShape fieldConverters  ->
                    let labelSearch = unionShape.Fields |> Array.map (fun f -> f.Label) |> BinSearch
                    (labelSearch, unionShape, fieldConverters))

            { new CborConverter<'T> with
                member _.Serialize writer value =
                    let tag = shape.GetTag value
                    let _, caseInfo, fieldConverters = unionCases.[tag]
                    if fieldConverters.Length = 0 then
                        writer.WriteTextString caseInfo.CaseInfo.Name
                    else
                        writer.WriteStartMap (1 + caseInfo.Fields.Length) // $case label + fields
                        writer.WriteTextString CborHelpers.FSharpUnionCaseNameFieldName
                        writer.WriteTextString caseInfo.CaseInfo.Name
                        for fieldConverter in fieldConverters do
                            writer.WriteTextString fieldConverter.PropertyName
                            fieldConverter.SerializeField(writer, &value)
                        writer.WriteEndMap()

                member _.Deserialize reader =
                    match reader.PeekState() with
                    | CborReaderState.TextString ->
                        let caseName = reader.ReadTextString()
                        let tag = caseNameSearch.TryFindIndex caseName
                        if tag < 0 then CborHelpers.throwUnrecognizedFSharpUnionCaseName typeof<'T> caseName
                        let _,caseInfo,_ = unionCases.[tag]
                        caseInfo.CreateUninitialized()
                    | CborReaderState.StartMap ->
                        let _ = reader.ReadStartMap()
                        if reader.ReadTextString() <> CborHelpers.FSharpUnionCaseNameFieldName then
                            CborHelpers.throwMissingUnionCaseField()

                        let caseName = reader.ReadTextString()
                        let tag = caseNameSearch.TryFindIndex caseName
                        if tag < 0 then CborHelpers.throwUnrecognizedFSharpUnionCaseName typeof<'T> caseName
                        let labelSearch, caseInfo, fieldConverters = unionCases.[tag]

                        let mutable result = caseInfo.CreateUninitialized()
                        while reader.PeekState() <> CborReaderState.EndMap do
                            let key = reader.ReadTextString() // TODO spanify key reads
                            let fieldIndex = labelSearch.TryFindIndex key
                            if fieldIndex < 0 then reader.SkipValue() else
                            let fieldConverter = fieldConverters.[fieldIndex]
                            fieldConverter.DeserializeField(reader, &result)
                        reader.ReadEndMap()
                        result
                    | token -> CborHelpers.throwInvalidToken token }
            |> HKT.pack

    interface ICliMutableBuilder<CborConverter, CborFieldConverter> with
        member _.CliMutable shape (HKT.Unpacks fieldConverters) =
            let labelSearch = fieldConverters |> Seq.map (fun c -> c.PropertyName) |> BinSearch
            { new CborConverter<'T> with
                member _.Serialize writer value =
                    writer.WriteStartMap shape.Properties.Length
                    for fieldConverter in fieldConverters do 
                        writer.WriteTextString fieldConverter.PropertyName
                        fieldConverter.SerializeField(writer, &value)
                    writer.WriteEndMap()

                member _.Deserialize reader =
                    let _ = reader.ReadStartMap()
                    let mutable result = shape.CreateUninitialized()
                    while reader.PeekState() <> CborReaderState.EndMap do
                        let key = reader.ReadTextString() // TODO spanify key reads
                        let fieldIndex = labelSearch.TryFindIndex key
                        if fieldIndex < 0 then reader.SkipValue() else
                        let fp = fieldConverters.[fieldIndex]
                        fp.DeserializeField(reader, &result)
                    reader.ReadEndMap()
                    result }
            |> HKT.pack

    interface IFieldExtractor<CborConverter, CborFieldConverter> with
        member _.Field shape (HKT.Unpack fieldConverter) = 
            { new CborFieldConverter<'DeclaringType> with
                member _.PropertyName = shape.Label
                member _.SerializeField (writer, value) =
                    let field = shape.GetByRef &value
                    fieldConverter.Serialize writer field

                member _.DeserializeField (reader, value) =
                    let field = fieldConverter.Deserialize reader
                    shape.SetByRef(&value, field) }
            |> HKT.pack

/// Tagless-final converter folding logic
type ConverterGenerator<'CborConverterBuilder 
                            when 'CborConverterBuilder :> CborConverterBuilder
                            and 'CborConverterBuilder : (new : unit -> 'CborConverterBuilder)>() =

    let cache = Some (new TypeCache())

    member val Builder = new 'CborConverterBuilder()
    member this.GenerateConverter<'t> () : CborConverter<'t> = FoldContext.fold cache this |> HKT.unpack

    abstract Fold<'t> : self:IGenericProgram<CborConverter> -> App<CborConverter, 't>
    default this.Fold<'t> self =
        match shapeof<'t> with
        | Fold.ByteArray this.Builder s -> s // byte[] takes precedence over 't[] converters
        | Fold.FSharpType this.Builder self s -> s
        | Fold.CliMutable this.Builder self s -> s
        | _ -> failwith $"Serialization for type {typeof<'t>} is not supported."

    interface IFoldContext<CborConverter> with 
        member this.Fold<'t> self = this.Fold<'t> self
        member this.Delay converterRef =
            let c = lazy(HKT.unpack converterRef.Value)
            { new CborConverter<'T> with
                member _.Serialize writer value = c.Value.Serialize writer value
                member _.Deserialize reader = c.Value.Deserialize reader }
            |> HKT.pack

//-----------------------------------
// Root-level serialization functions

let encode (converter : CborConverter<'T>) (value : 'T) : byte[] =
    let writer = new CborWriter(CborConformanceMode.Lax, convertIndefiniteLengthEncodings = false, allowMultipleRootLevelValues = false)
    converter.Serialize writer value
    writer.Encode()

let decode (converter : CborConverter<'T>) (cborEncoding : byte[]) : 'T =
    let reader = new CborReader(ReadOnlyMemory cborEncoding, CborConformanceMode.Lax, allowMultipleRootLevelValues = false)
    converter.Deserialize reader

let encodeHex (converter : CborConverter<'T>) (value : 'T) : string =
    let encoding = encode converter value
    bytesToHex (ReadOnlySpan encoding)

let decodeHex (converter : CborConverter<'T>) (cborHexEncoding : string) : 'T =
    let buffer = ArrayPool<byte>.Shared.Rent(cborHexEncoding.Length / 2 + 1)
    try
        hexToBytes (cborHexEncoding.AsSpan()) (Span buffer)
        let reader = new CborReader(ReadOnlyMemory buffer, CborConformanceMode.Lax, allowMultipleRootLevelValues = false)
        converter.Deserialize reader
    finally
        ArrayPool<byte>.Shared.Return buffer

//--------------------------
// Examples    

let private generator = new ConverterGenerator<CborConverterBuilder>()
let generateConverter<'t>() = generator.GenerateConverter<'t>()

let p0 = generateConverter<int>()

// Use http://cbor.me/ to validate encodings

encodeHex p0 42 // 182A
|> decodeHex p0

let p1 = generateConverter<int * int list>()

encodeHex p1 (1, [2;3;4]) // 820183020304
|> decodeHex p1

let p2 = generateConverter<int * string list option * string ref>()

encodeHex p2 (1, Some(["a";"b";"c"]), ref "foo") // 83018361616162616363666F6F
|> decodeHex p2

type Bar =
    | Foo of values: {| A : int ; B : string |}
    | Bar of number:int
    | C
    | D of item:string option
    | E of items:Map<string, int>

let p3 = generateConverter<Bar list []>()

// 8480826143A26524636173656144646974656DF681A265246361736563466F6F6676616C756573A26141182A614262313281A26524636173656145656974656D73A163666F6F182A
encodeHex p3 [| [] ; [C ; D None] ; [Foo {| A = 42 ; B = "12" |}] ; [E (Map.ofList [("foo", 42)])]|]
|> decodeHex p3

// Recursive type serialization
type BinTree<'T> = Leaf | Node of value:'T * left:BinTree<'T> * right:BinTree<'T>

let p4 = generateConverter<BinTree<int>> ()

// A4652463617365644E6F64656576616C756503646C656674A4652463617365644E6F64656576616C756501646C656674644C656166657269676874A4652463617365644E6F64656576616C756502646C656674644C656166657269676874644C656166657269676874644C656166
encodeHex p4 (Node(3, Node(1, Leaf, Node(2, Leaf,Leaf)), Leaf))
|> decodeHex p4


//--------------------------
// Extensibility

type ConverterBuilderExtensions() =
    inherit CborConverterBuilder()

    // add support for dictionary converters
    interface IDictionaryBuilder<CborConverter> with
        member _.Dictionary (HKT.Unpack keyConverter) (HKT.Unpack valueConverter) =
            { new CborConverter<Dictionary<'key, 'value>> with
                member _.Serialize writer dict =
                    writer.WriteStartMap dict.Count
                    for kv in dict do
                        keyConverter.Serialize writer kv.Key
                        valueConverter.Serialize writer kv.Value
                    writer.WriteEndMap()

                member _.Deserialize reader =
                    let size = reader.ReadStartMap()
                    let dict = 
                        if size.HasValue then new Dictionary<'key, 'value>(size.Value) 
                        else new Dictionary<'key, 'value>()

                    while reader.PeekState() <> CborReaderState.EndMap do
                        let key = keyConverter.Deserialize reader
                        let value = valueConverter.Deserialize reader
                        dict.Add(key, value)
                    reader.ReadEndMap()
                    dict }
            |> HKT.pack

type ConverterGeneratorExtensions() =
    inherit ConverterGenerator<ConverterBuilderExtensions>()
    override this.Fold<'t> self =
        match shapeof<'t> with
        | Fold.Dictionary this.Builder self s -> s
        | _ -> base.Fold<'t> self

let generatorExtensions = new ConverterGeneratorExtensions()
let dictConverter = generatorExtensions.GenerateConverter<Dictionary<string, bigint>>()

// A1623432C2412A
encodeHex dictConverter (let dict = new Dictionary<string, bigint>() in dict.Add("42", 42I) ; dict)
|> decodeHex dictConverter