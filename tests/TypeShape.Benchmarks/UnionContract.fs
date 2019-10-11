module TypeShape.Benchmarks.UnionEncoder

open System
open System.Runtime.Serialization
open FSharp.Reflection
open TypeShape.UnionContract
open BenchmarkDotNet.Attributes

// Placeholder event records emulating an imaginary domain
type CartCreated = { id: string ; client_id: string } 
type AddressUpdated = { id: string ; address: string }
type ItemReturnWaived = { id : string ; item_id : string ; value: bool }
type CashUsed = { id: string ; value: bool }
type CreditsUsed = { id: string ; value: bool }
type CartItemRemoved = { id : string ; retail_sku_id : string ; date : DateTimeOffset }

type TestUnionContract =
    | NullaryEvent of unit
    | CartCreated of CartCreated
    | AddressUpdated of AddressUpdated
    | ItemReturnWaived of ItemReturnWaived
    | CashUsed of CashUsed
    | CreditsUsed of CreditsUsed
    | CartItemRemoved of CartItemRemoved
    | InlinedCase of value:int
    | [<DataMember(Name = "Legacy")>] Old_Stuff of CartItemRemoved
with
    interface IUnionContract


/// Union encoder/decoder interface
type IUnionEncoder =
    abstract Encode : TestUnionContract -> EncodedUnion<obj>
    abstract Decode : EncodedUnion<obj> -> TestUnionContract

// baseline bespoke implementation
let baselineEncoder =
    { new IUnionEncoder with
        member __.Encode(sum : TestUnionContract): EncodedUnion<obj> = 
            let inline mkEnc name payload = { CaseName = name ; Payload = box payload }
            match sum with
            | NullaryEvent () -> mkEnc "NullaryEvent" ()
            | CartCreated c -> mkEnc "CartCreated" c
            | AddressUpdated c -> mkEnc "AddressUpdated" c
            | ItemReturnWaived c -> mkEnc "ItemReturnWaived" c
            | CashUsed c -> mkEnc "CashUsed" c
            | CreditsUsed c -> mkEnc "CreditsUsed" c
            | CartItemRemoved c -> mkEnc "CartItemRemoved" c
            | InlinedCase c -> mkEnc "InlinedCase" c
            | Old_Stuff c -> mkEnc "Legacy" c

        member __.Decode(e : EncodedUnion<obj>): TestUnionContract = 
            let inline getValue() = e.Payload :?> 'a

            match e.CaseName with
            | "NullaryEvent" -> NullaryEvent(getValue())
            | "CartCreated"  -> CartCreated(getValue())
            | "AddressUpdated" -> AddressUpdated(getValue())
            | "ItemReturnWaived" -> ItemReturnWaived(getValue())
            | "CashUsed" -> CashUsed(getValue())
            | "CreditsUsed" -> CreditsUsed(getValue())
            | "CartItemRemoved" -> CartItemRemoved(getValue())
            | "InlinedCase" -> InlinedCase(getValue())
            | "Legacy" -> Old_Stuff(getValue())
            | _ -> failwith "unrecognized case name"
    }

// reflection-driven union encoder
let reflectionEncoder =
    let tagReader = FSharpValue.PreComputeUnionTagReader(typeof<TestUnionContract>)
    let ucis = FSharpType.GetUnionCases(typeof<TestUnionContract>)
    let ctors = ucis |> Array.map FSharpValue.PreComputeUnionConstructor
    let readers = ucis |> Array.map FSharpValue.PreComputeUnionReader
    { new IUnionEncoder with
        member __.Encode(sum : TestUnionContract): EncodedUnion<obj> = 
            let tag = tagReader sum
            let name = ucis.[tag].Name
            let reader = readers.[tag]
            let values = reader sum
            { CaseName = name ; Payload = values.[0] }

        member __.Decode(e : EncodedUnion<obj>): TestUnionContract =
            let tag = ucis |> Array.findIndex (fun u -> e.CaseName = u.Name)
            let ctor = ctors.[tag]
            let values = [|e.Payload|]
            ctor values :?> TestUnionContract
    }

// typeshape-driven encoder
let typeShapeEncoder = 
    let encoder = UnionContractEncoder.Create<TestUnionContract,_>(BoxEncoder()) 
    { new IUnionEncoder with
        member __.Encode(sum : TestUnionContract): EncodedUnion<obj> = 
            encoder.Encode sum

        member __.Decode(e : EncodedUnion<obj>): TestUnionContract =
            encoder.Decode e }

let testValues =
    [|  NullaryEvent ()
        CartCreated { id = "cartId" ; client_id = "clientId" }
        AddressUpdated { id = "clientId" ; address = "address" }
        ItemReturnWaived { id = "cartId" ; item_id = "itemId" ; value = false }
        CashUsed { id = "cartId" ; value = true }
        CartItemRemoved { id = "cartId" ; retail_sku_id = "" ; date = DateTimeOffset() }
        CreditsUsed { id = "customerId" ; value = true }
        InlinedCase 42
        CartCreated { id = "cartId" ; client_id = "clientId" }
        CashUsed { id = "cashId" ; value = false } 
        Old_Stuff { id = "cartId" ; retail_sku_id = "" ; date = DateTimeOffset() } |]

let inline testEncoderRoundtrip (encoder : IUnionEncoder) =
    for v in testValues do
        let e = encoder.Encode v
        let _ = encoder.Decode e
        ()

type UnionContractBenchmark() =
    
    [<Benchmark(Description = "Baseline Union Encoder", Baseline = true)>]
    member __.Baseline() = testEncoderRoundtrip baselineEncoder
    [<Benchmark(Description = "Reflection Union Encoder")>]
    member __.Reflection() = testEncoderRoundtrip reflectionEncoder
    [<Benchmark(Description = "TypeShape Union Encoder")>]
    member __.TypeShape() = testEncoderRoundtrip typeShapeEncoder