module TypeShape.Benchmarks.UnionEncoder

open System
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
with
    interface IUnionContract

// baseline bespoke implementation
type BaselineEncoder () =
    member _.Encode(sum : TestUnionContract): EncodedUnion<obj> = 
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

    member _.Decode(e : EncodedUnion<obj>): TestUnionContract = 
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
        | _ -> failwith "unrecognized case name"

// reflection-driven union encoder
type ReflectionEncoder() =
    let tagReader = FSharpValue.PreComputeUnionTagReader(typeof<TestUnionContract>)
    let ucis = FSharpType.GetUnionCases(typeof<TestUnionContract>)
    let ctors = ucis |> Array.map FSharpValue.PreComputeUnionConstructor
    let readers = ucis |> Array.map FSharpValue.PreComputeUnionReader

    member _.Encode(sum : TestUnionContract): EncodedUnion<obj> = 
        let tag = tagReader sum
        let name = ucis.[tag].Name
        let reader = readers.[tag]
        let values = reader sum
        { CaseName = name ; Payload = values.[0] }

    member _.Decode(e : EncodedUnion<obj>): TestUnionContract =
        let tag = ucis |> Array.findIndex (fun u -> e.CaseName = u.Name)
        let ctor = ctors.[tag]
        let values = [|e.Payload|]
        ctor values :?> TestUnionContract

// typeshape-driven encoder
let mkTypeShapeEncoder() = UnionContractEncoder.Create<TestUnionContract,_>(BoxEncoder()) 

[<Literal>]
let TestValueCount = 10
let mkTestValues () =
    [|  NullaryEvent ()
        CartCreated { id = "cartId" ; client_id = "clientId" }
        AddressUpdated { id = "clientId" ; address = "address" }
        ItemReturnWaived { id = "cartId" ; item_id = "itemId" ; value = false }
        CashUsed { id = "cartId" ; value = true }
        CartItemRemoved { id = "cartId" ; retail_sku_id = "" ; date = DateTimeOffset() }
        CreditsUsed { id = "customerId" ; value = true }
        InlinedCase 42
        CartCreated { id = "cartId" ; client_id = "clientId" }
        CashUsed { id = "cashId" ; value = false } |]

[<MemoryDiagnoser>]
type UnionContractBenchmark() =
    let baselineEncoder = new BaselineEncoder()
    let reflectionEncoder = new ReflectionEncoder()
    let typeShapeEncoder = mkTypeShapeEncoder()

    let testValues = mkTestValues()
    let encodedValues = testValues |> Array.map baselineEncoder.Encode

    do if testValues.Length <> TestValueCount then invalidOp $"please update the value of {nameof(TestValueCount)} to be {testValues.Length}."
    
    [<Benchmark(OperationsPerInvoke = TestValueCount)>]
    member _.Serialize_Baseline() = for v in testValues do baselineEncoder.Encode v |> ignore
    [<Benchmark(OperationsPerInvoke = TestValueCount)>]
    member _.Serialize_Reflection() = for v in testValues do reflectionEncoder.Encode v |> ignore
    [<Benchmark(OperationsPerInvoke = TestValueCount)>]
    member _.Serialize_TypeShape() = for v in testValues do typeShapeEncoder.Encode v |> ignore

    [<Benchmark(OperationsPerInvoke = TestValueCount)>]
    member _.Deserialize_Baseline() = for e in encodedValues do baselineEncoder.Decode e |> ignore
    [<Benchmark(OperationsPerInvoke = TestValueCount)>]
    member _.Deserialize_Reflection() = for e in encodedValues do reflectionEncoder.Decode e |> ignore
    [<Benchmark(OperationsPerInvoke = TestValueCount)>]
    member _.Deserialize_TypeShape() = for e in encodedValues do typeShapeEncoder.Decode e |> ignore
