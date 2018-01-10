module TypeShape.Tests.EventSum

open System
open System.Runtime.Serialization
open FSharp.Reflection
open Xunit
open FsCheck.Xunit
open Swensen.Unquote
open TypeShape.Empty
open TypeShape.EventSum

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
    | [<DataMember(Name = "Legacy")>] Old_Stuff of CartItemRemoved
    
let mkCastEncoder<'T> = generateSumEventEncoder<'T,obj> (CastEncoder())
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

    test <@ expectedLabel = e.EventType @>

[<Fact>]
let ``Should throw FormatException on Decode of unrecognized event types`` () =
    let enc = encoder.Value
    let e = enc.Encode (CartCreated empty)
    let e' = { e with EventType = "__UNKNOWN_TYPE__" }
    raises<FormatException> <@ enc.Decode e' @>

[<Fact>]
let ``Should return None on TryDecode of unrecognized event types`` () =
    let enc = encoder.Value
    let e = enc.Encode (CartCreated empty)
    let e' = { e with EventType = "__UNKNOWN_TYPE__" }
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



type EventSumWithMultipleFields =
    | A of CreditsUsed
    | B of AddressUpdated
    | C of CartCreated * CreditsUsed

[<Fact>]
let ``Should fail on event sums with multiple fields`` () =
    raises<ArgumentException> <@ mkCastEncoder<EventSumWithMultipleFields> @>

type EventSumWithNonRecordCase =
    | A of CartCreated
    | B of CashUsed
    | C of string

[<Fact>]
let ``Should fail on event sums with non-record cases`` () =
    raises<ArgumentException> <@ mkCastEncoder<EventSumWithNonRecordCase> @>


type EventSumWithDuplicateLabels =
    | A of CartCreated
    | B of CashUsed
    | [<DataMember(Name = "A")>] C of string

[<Fact>]
let ``Should fail on event sums with duplicate labels`` () =
    raises<ArgumentException> <@ mkCastEncoder<EventSumWithDuplicateLabels> @>