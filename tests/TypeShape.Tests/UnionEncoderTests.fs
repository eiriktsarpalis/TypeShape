module TypeShape.Tests.UnionEncoder

open System
open System.Runtime.Serialization
open FSharp.Reflection
open Xunit
open FsCheck.Xunit
open Swensen.Unquote
open TypeShape.Empty
open TypeShape.UnionEncoder

// Placeholder event records emulating an imaginary domain
type CartCreated = { id: string ; client_id: string } 
type AddressUpdated = { id: string ; address: string }
type ItemReturnWaived = { id : string ; item_id : string ; value: bool }
type CashUsed = { id: string ; value: bool }
type CreditsUsed = { id: string ; value: bool }
type CartItemRemoved = { id : string ; retail_sku_id : string ; date : DateTimeOffset }

type BasicEventSum =
    | NullaryEvent
    | CartCreated of CartCreated
    | AddressUpdated of AddressUpdated
    | ItemReturnWaived of ItemReturnWaived
    | CashUsed of CashUsed
    | CreditsUsed of CreditsUsed
    | CartItemRemoved of CartItemRemoved
    | InlinedCase of value1:int * value2:string
    | [<DataMember(Name = "Legacy")>] Old_Stuff of CartItemRemoved
    
let mkCastEncoder<'T> = UnionEncoder<'T>.Create(CastEncoder())
let encoder = lazy(mkCastEncoder<BasicEventSum>)

[<Property>]
let ``Basic EventSum enc/dec roundtrip`` (sum : BasicEventSum) =
    let e = encoder.Value
    e.Decode(e.Encode sum) = sum

[<Property>]
let ``Should use correct event types in encodings`` (sum : BasicEventSum) =
    let e = encoder.Value.Encode sum
    let expectedLabel =
        match sum with
        | Old_Stuff _ -> "Legacy"
        | _ ->
            let uci, _ = FSharpValue.GetUnionFields(sum, typeof<BasicEventSum>, true)
            uci.Name

    test <@ expectedLabel = e.CaseName @>

[<Property>]
let ``Payload should match arity of union case`` (sum : BasicEventSum) =
    let enc = encoder.Value
    let encoding = enc.Encode sum
    let _, fields = FSharpValue.GetUnionFields(sum, typeof<BasicEventSum>, true)
    let expectedArity = fields.Length
    test <@ expectedArity = Array.length encoding.Payload @>

[<Property>]
let ``Payload keys should match case names`` (sum : BasicEventSum) =
    let enc = encoder.Value
    let encoding = enc.Encode sum
    let uci, _ = FSharpValue.GetUnionFields(sum, typeof<BasicEventSum>, true)
    let expectedKeys = uci.GetFields() |> Array.map (fun f -> f.Name)
    let actualKeys = encoding.Payload |> Array.map (fun kv -> kv.Key)
    test <@ expectedKeys = actualKeys @>

[<Fact>]
let ``Nullary union cases should return empty payloads`` () =
    let enc = encoder.Value
    let encoding = enc.Encode NullaryEvent
    test <@ Array.isEmpty encoding.Payload @>


[<Fact>]
let ``Should throw FormatException on Decode of unrecognized event types`` () =
    let enc = encoder.Value
    let e = enc.Encode (CartCreated empty)
    let e' = { e with CaseName = "__UNKNOWN_TYPE__" }
    raises<FormatException> <@ enc.Decode e' @>

[<Fact>]
let ``Should return None on TryDecode of unrecognized event types`` () =
    let enc = encoder.Value
    let e = enc.Encode (CartCreated empty)
    let e' = { e with CaseName = "__UNKNOWN_TYPE__" }
    test <@ None = enc.TryDecode e' @>


type private PrivateEventSum = A of CartCreated

[<Fact>]
let ``Should work with private types`` () =
    let enc = mkCastEncoder<PrivateEventSum>
    let value = A empty
    test <@ value = enc.Decode(enc.Encode value) @>


[<Fact>]
let ``Should fail on non-union inputs`` () =
    raises<ArgumentException> <@ mkCastEncoder<CartCreated> @>

type EventSumWithDuplicateLabels =
    | A of CartCreated
    | B of CashUsed
    | [<DataMember(Name = "A")>] C of string

[<Fact>]
let ``Should fail on event sums with duplicate labels`` () =
    raises<ArgumentException> <@ mkCastEncoder<EventSumWithDuplicateLabels> @>