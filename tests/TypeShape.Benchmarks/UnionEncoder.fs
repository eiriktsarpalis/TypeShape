module TypeShape.Benchmarks.UnionEncoder

open System
open System.Reflection
open System.Runtime.Serialization
open System.Collections.Generic
open FSharp.Reflection
open FsCheck
open TypeShape.UnionEncoder
open BenchmarkDotNet.Attributes

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

let baselineEncoder =
    { new IUnionEncoder<BasicEventSum, obj> with
        member this.Encode(sum : BasicEventSum): EncodedUnion<obj> = 
            let inline (=>) k v = KeyValuePair(k,box v)
            let mkEnc name payload = { CaseName = name ; Payload = payload }
            match sum with
            | NullaryEvent -> mkEnc "NullaryEvent" [||]
            | CartCreated c -> mkEnc "CartCreated" [|"Item" => c|]
            | AddressUpdated c -> mkEnc "AddressUpdated" [|"Item" => c |]
            | ItemReturnWaived c -> mkEnc "ItemReturnWaived" [|"Item" => c|]
            | CashUsed c -> mkEnc "CashUsed" [|"Item" => c |]
            | CreditsUsed c -> mkEnc "CreditsUsed" [|"Item" => c |]
            | CartItemRemoved c -> mkEnc "CartItemRemoved" [|"Item" => c |]
            | InlinedCase (value1, value2) ->
                mkEnc "InlinedCase" [| "value1" => value1 ; "value2" => value2 |]
            | Old_Stuff c -> mkEnc "Legacy" [|"Item" => c |]

        member this.Decode(e : EncodedUnion<obj>): BasicEventSum = 
            let getValue key = 
                let kv = e.Payload |> Array.find (fun kv -> kv.Key = key)
                kv.Value :?> 'a

            match e.CaseName with
            | "NullaryEvent" -> NullaryEvent
            | "CartCreated"  -> CartCreated(getValue "Item")
            | "AddressUpdated" -> AddressUpdated(getValue "Item")
            | "ItemReturnWaived" -> ItemReturnWaived(getValue "Item")
            | "CashUsed" -> CashUsed(getValue "Item")
            | "CreditsUsed" -> CreditsUsed(getValue "Item")
            | "CartItemRemoved" -> CartItemRemoved(getValue "Item")
            | "InlinedCase" -> InlinedCase(getValue "value1", getValue "value2")
            | "Legacy" -> Old_Stuff(getValue "Item")
            | _ -> failwith "unrecognized case name"

        member this.GetCaseSchema(arg1: BasicEventSum) =
            raise (System.NotImplementedException())

        member this.CaseSchemas = 
            raise (System.NotImplementedException())

        member this.TryDecode(arg1: EncodedUnion<obj>): BasicEventSum option = 
            raise (System.NotImplementedException())
    }

let reflectionEncoder =
    let tagReader = FSharpValue.PreComputeUnionTagReader(typeof<BasicEventSum>)
    let ucis = FSharpType.GetUnionCases(typeof<BasicEventSum>)
    let fieldss = ucis |> Array.map (fun u -> u.GetFields() |> Array.map (fun f -> f.Name))
    let ctors = ucis |> Array.map FSharpValue.PreComputeUnionConstructor
    let readers = ucis |> Array.map FSharpValue.PreComputeUnionReader
    { new IUnionEncoder<BasicEventSum, obj> with
        member this.Encode(sum : BasicEventSum): EncodedUnion<obj> = 
            let tag = tagReader sum
            let name = ucis.[tag].Name
            let reader = readers.[tag]
            let fields = fieldss.[tag]
            let values = reader sum
            let keyVals = (fields,values) ||> Array.map2 (fun k v -> KeyValuePair(k,v))
            { CaseName = name ; Payload = keyVals }

        member this.Decode(e : EncodedUnion<obj>): BasicEventSum =
            let tag = ucis |> Array.findIndex (fun u -> e.CaseName = u.Name)
            let ctor = ctors.[tag]
            let fields = fieldss.[tag]
            let values = fields |> Array.map (fun f -> let kv = e.Payload |> Array.find (fun kv -> kv.Key = f) in kv.Value)
            ctor values :?> BasicEventSum

        member this.GetCaseSchema(arg1: BasicEventSum) =
            raise (System.NotImplementedException())

        member this.CaseSchemas = 
            raise (System.NotImplementedException())

        member this.TryDecode(arg1: EncodedUnion<obj>): BasicEventSum option = 
            raise (System.NotImplementedException())
    }

let typeShapeEncoder = UnionEncoder<BasicEventSum>.Create(CastEncoder())

let valuePool =
    let values =
        Arb.from<BasicEventSum>.Generator 
        |> Gen.sampleWithSeed (Rnd(2018UL)) 10 20

    let i = ref 0
    fun () ->
        let v = values.[!i % values.Length]
        do incr i
        v

let inline testEncoderRoundtrip (encoder : IUnionEncoder<BasicEventSum, obj>) =
    let v = valuePool()
    let e = encoder.Encode v
    let v' = encoder.Decode e
    ()
    

type UnionEncoderBenchmark() =
    
    [<Benchmark(Description = "Baseline Union Encoder", Baseline = true)>]
    member __.Baseline() = testEncoderRoundtrip baselineEncoder
    [<Benchmark(Description = "Reflection Union Encoder")>]
    member __.Reflection() = testEncoderRoundtrip reflectionEncoder
    [<Benchmark(Description = "TypeShape Union Encoder")>]
    member __.TypeShape() = testEncoderRoundtrip typeShapeEncoder