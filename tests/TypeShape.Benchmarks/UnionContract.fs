module TypeShape.Benchmarks.UnionEncoder

open System
open System.Reflection
open System.Runtime.Serialization
open System.Collections.Generic
open FSharp.Reflection
open FsCheck
open TypeShape.UnionContract
open BenchmarkDotNet.Attributes

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
    | InlinedCase of value:int
    | [<DataMember(Name = "Legacy")>] Old_Stuff of CartItemRemoved
with
    interface IUnionContract


type IEncoder =
    abstract Encode : BasicEventSum -> EncodedUnion<obj>
    abstract Decode : EncodedUnion<obj> -> BasicEventSum

let baselineEncoder =
    { new IEncoder with
        member this.Encode(sum : BasicEventSum): EncodedUnion<obj> = 
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

        member this.Decode(e : EncodedUnion<obj>): BasicEventSum = 
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

let reflectionEncoder =
    let tagReader = FSharpValue.PreComputeUnionTagReader(typeof<BasicEventSum>)
    let ucis = FSharpType.GetUnionCases(typeof<BasicEventSum>)
    let fieldss = ucis |> Array.map (fun u -> u.GetFields() |> Array.map (fun f -> f.Name))
    let ctors = ucis |> Array.map FSharpValue.PreComputeUnionConstructor
    let readers = ucis |> Array.map FSharpValue.PreComputeUnionReader
    { new IEncoder with
        member this.Encode(sum : BasicEventSum): EncodedUnion<obj> = 
            let tag = tagReader sum
            let name = ucis.[tag].Name
            let reader = readers.[tag]
            let values = reader sum
            { CaseName = name ; Payload = values.[0] }

        member this.Decode(e : EncodedUnion<obj>): BasicEventSum =
            let tag = ucis |> Array.findIndex (fun u -> e.CaseName = u.Name)
            let ctor = ctors.[tag]
            let values = [|e.Payload|]
            ctor values :?> BasicEventSum
    }

let typeShapeEncoder = 
    let encoder = UnionContractEncoder.Create<BasicEventSum,_>(BoxEncoder()) 
    { new IEncoder with
        member this.Encode(sum : BasicEventSum): EncodedUnion<obj> = 
            encoder.Encode sum

        member this.Decode(e : EncodedUnion<obj>): BasicEventSum =
            encoder.Decode e }

let valuePool =
    let values =
        Arb.from<BasicEventSum>.Generator 
        |> Gen.sampleWithSeed (Rnd(2018UL)) 10 20

    let i = ref 0
    fun () ->
        let v = values.[!i % values.Length]
        do incr i
        v

let inline testEncoderRoundtrip (encoder : IEncoder) =
    let v = valuePool()
    let e = encoder.Encode v
    let v' = encoder.Decode e
    ()
    

type UnionContractBenchmark() =
    
    [<Benchmark(Description = "Baseline Union Encoder", Baseline = true)>]
    member __.Baseline() = testEncoderRoundtrip baselineEncoder
    [<Benchmark(Description = "Reflection Union Encoder")>]
    member __.Reflection() = testEncoderRoundtrip reflectionEncoder
    [<Benchmark(Description = "TypeShape Union Encoder")>]
    member __.TypeShape() = testEncoderRoundtrip typeShapeEncoder