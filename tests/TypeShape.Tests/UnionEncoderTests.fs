module TypeShape.Tests.UnionEncoder

open System
open System.Collections.Generic
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
    | NullaryEvent of unit
    | CartCreated of CartCreated
    | AddressUpdated of AddressUpdated
    | ItemReturnWaived of ItemReturnWaived
    | CashUsed of CashUsed
    | CreditsUsed of CreditsUsed
    | CartItemRemoved of CartItemRemoved
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

[<Fact>]
let ``Should fail when requiring record fields`` () =
    raises<ArgumentException> <@ UnionEncoder<BasicEventSum>.Create(CastEncoder(), requireRecordFields = true) @>

type private PrivateEventSum = A of CartCreated

[<Fact>]
let ``Should work with private types`` () =
    let enc = mkCastEncoder<PrivateEventSum>
    let value = A empty
    test <@ value = enc.Decode(enc.Encode value) @>


type EventSumWithNullaryCase =
    | Nullary
    | A of CartCreated


[<Fact>]
let ``Should fail on nullary union cases`` () =
    raises<ArgumentException> <@ mkCastEncoder<EventSumWithNullaryCase> @>


type EventSumWithLargeArity =
    | A of CartCreated
    | B of CartCreated * CartCreated

[<Fact>]
let ``Should fail on union case arities > 1`` () =
    raises<ArgumentException> <@ mkCastEncoder<EventSumWithLargeArity> @>


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