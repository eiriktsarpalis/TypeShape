module TypeShape.HKT.Pickler

// Defines a simplistic generic pickling implementation
// that serializes and deserializes arbitrary F# values to streams of tokens

type Pickler<'T> =
    {
        pickle   : (Token -> unit) -> 'T -> unit
        unpickle : (bool -> Token) -> 'T
    }

and Token =
    | Null
    | Bool of bool
    | Number of decimal
    | String of string
    | Label of string
    | StartObject
    | EndObject
    | StartArray
    | EndArray

//--------------------------------------------------

and Pickler =
    static member Assign(_ : App<Pickler, 'a>, _ : Pickler<'a>) = ()


type FieldPickler<'T> = 
    { 
        label : string
        fpickle : (Token -> unit) -> 'T -> unit
        funpickle : (bool -> Token) -> 'T -> 'T
    }

and FieldPickler =
    static member Assign(_ : App<FieldPickler, 'a>, _ : FieldPickler<'a>) = ()

type PickerBuilder() =
    let invalidTok tok = failwithf "invalid token sequence %A" tok

    let (|AsNumber|_|) (tok : Token) =
        match tok with
        | Null -> Some 0m
        | Bool b -> Some (if b then 0m else 1m)
        | Number i -> Some i
        | String s ->
            let ok,f = System.Decimal.TryParse s
            if ok then Some f else None
        | _ -> None

    interface ITypeBuilder<Pickler, FieldPickler> with
        member __.Bool () = 
            HKT.pack {  
                pickle = fun k b -> k (Bool b)
                unpickle = fun r -> match r true with Bool b -> b | t -> invalidTok t
            }

        member __.Byte () =
            HKT.pack {
                pickle = fun k b -> k (Number (decimal b))
                unpickle = fun r -> match r true with AsNumber i -> byte i | t -> invalidTok t
            }

        member __.SByte() =
            HKT.pack {
                pickle = fun k b -> k (Number (decimal b))
                unpickle = fun r -> match r true with AsNumber i -> sbyte i | t -> invalidTok t
            }

        member __.Char() =
            HKT.pack {
                pickle = fun k c -> k (String (string c))
                unpickle = fun r -> match r true with String s -> s.[0] | t -> invalidTok t
            }

        member __.Int16 () =
            HKT.pack {
                pickle = fun k b -> k (Number (decimal b))
                unpickle = fun r -> match r true with AsNumber i -> int16 i | t -> invalidTok t
            }

        member __.Int32 () =
            HKT.pack {
                pickle = fun k b -> k (Number (decimal b))
                unpickle = fun r -> match r true with AsNumber i -> int i | t -> invalidTok t
            }

        member __.Int64 () =
            HKT.pack {
                pickle = fun k b -> k (Number (decimal b))
                unpickle = fun r -> match r true with AsNumber i -> int64 i | t -> invalidTok t
            }

        member __.UInt16 () =
            HKT.pack {
                pickle = fun k b -> k (Number (decimal b))
                unpickle = fun r -> match r true with AsNumber i -> uint16 i | t -> invalidTok t
            }

        member __.UInt32 () =
            HKT.pack {
                pickle = fun k b -> k (Number (decimal b))
                unpickle = fun r -> match r true with AsNumber i -> uint32 i | t -> invalidTok t
            }

        member __.UInt64 () =
            HKT.pack {
                pickle = fun k b -> k (Number (decimal b))
                unpickle = fun r -> match r true with AsNumber i -> uint64 i | t -> invalidTok t
            }

        member __.Single () =
            HKT.pack {
                pickle = fun k b -> k (Number (decimal b))
                unpickle = fun r -> match r true with AsNumber i -> single i | t -> invalidTok t
            }

        member __.Double () =
            HKT.pack {
                pickle = fun k b -> k (Number (decimal b))
                unpickle = fun r -> match r true with AsNumber i -> float i | t -> invalidTok t
            }

        member __.Decimal() =
            HKT.pack {
                pickle = fun k b -> k (Number (decimal b))
                unpickle = fun r -> match r true with AsNumber i -> i | t -> invalidTok t
            }

        member __.BigInt () =
            HKT.pack {
                pickle = fun k i -> k (String (i.ToString()))
                unpickle = fun r -> match r true with String s -> bigint.Parse s | AsNumber i -> bigint i | t -> invalidTok t
            }

        member __.Unit() =
            HKT.pack {
                pickle = fun k i -> k Null
                unpickle = fun r -> match r true with Null -> () | t -> invalidTok t
            }

        member __.String () =
            HKT.pack {
                pickle = fun k s -> k (String s)
                unpickle = fun r -> match r true with String s -> s | t -> invalidTok t
            }

        member __.Guid () =
            HKT.pack {
                pickle = fun k s -> k (String (s.ToString()))
                unpickle = fun r -> match r true with String s -> System.Guid.Parse s | t -> invalidTok t
            }

        member __.TimeSpan () =
            HKT.pack {
                pickle = fun k s -> k (String (s.ToString()))
                unpickle = fun r -> match r true with String s -> System.TimeSpan.Parse s | t -> invalidTok t
            }

        member __.DateTime () =
            HKT.pack {
                pickle = fun k s -> k (String (s.ToString("O")))
                unpickle = fun r -> match r true with String s -> System.DateTime.Parse s | t -> invalidTok t
            }

        member __.DateTimeOffset() =
            HKT.pack {
                pickle = fun k s -> k (String (s.ToString("O")))
                unpickle = fun r -> match r true with String s -> System.DateTimeOffset.Parse s | t -> invalidTok t
            }

        member __.Enum (HKT.Unpack ep) =
            HKT.pack {
                pickle = fun k e -> ep.pickle k (LanguagePrimitives.EnumToValue e)
                unpickle = fun r -> ep.unpickle r |> LanguagePrimitives.EnumOfValue
            }

        member __.Nullable (HKT.Unpack ep) = 
            HKT.pack {
                pickle = fun k e -> if e.HasValue then ep.pickle k e.Value else k Null
                unpickle = fun r -> match r false with Null -> r true |> ignore; System.Nullable() | _ -> System.Nullable(ep.unpickle r)
            }

        member __.Array (HKT.Unpack ep) = 
            HKT.pack {
                pickle = fun k xs -> k StartArray ; (for x in xs do ep.pickle k x); k EndArray
                unpickle = fun r -> 
                    match r true with 
                    | StartArray ->
                        let acc = ResizeArray()
                        let rec aux () =
                            match r false with
                            | EndArray -> r true |> ignore ; acc.ToArray()
                            | _ -> ep.unpickle r |> acc.Add ; aux()

                        aux()
                    | t -> invalidTok t
            }

        member __.Option (HKT.Unpack ep) =
            HKT.pack {
                pickle = fun k xs -> match xs with None -> k Null | Some x -> ep.pickle k x
                unpickle = fun r ->
                    match r false with
                    | Null -> r true |> ignore ; None
                    | _ -> ep.unpickle r |> Some
            }

        member __.List (HKT.Unpack ep) =
            HKT.pack {
                pickle = fun k xs -> k StartArray ; (for x in xs do ep.pickle k x); k EndArray
                unpickle = fun r -> 
                    match r true with 
                    | StartArray ->
                        let acc = ResizeArray()
                        let rec aux () =
                            match r false with
                            | EndArray -> r true |> ignore ; Seq.toList acc
                            | _ -> ep.unpickle r |> acc.Add ; aux()

                        aux()
                    | t -> invalidTok t
            }

        member __.Set (HKT.Unpack ep) =
            HKT.pack {
                pickle = fun k xs -> k StartArray ; (for x in xs do ep.pickle k x); k EndArray
                unpickle = fun r -> 
                    match r true with 
                    | StartArray ->
                        let acc = ResizeArray()
                        let rec aux () =
                            match r false with
                            | EndArray -> r true |> ignore; Set.ofSeq acc
                            | _ -> ep.unpickle r |> acc.Add ; aux()

                        aux()
                    | t -> invalidTok t
            }

        member __.Map (HKT.Unpack kp) (HKT.Unpack vp) =
            match box kp with
            | :? Pickler<string> ->
                (HKT.pack << unbox) { 
                    pickle = fun k (m : Map<_,_>) -> k StartObject ; (for x in m do k (Label x.Key) ; vp.pickle k x.Value) ; k EndObject
                    unpickle = fun r ->
                        match r true with
                        | StartObject ->
                            let acc = ResizeArray()
                            let rec aux () =
                                match r true with
                                | EndObject -> Map.ofSeq acc
                                | Label l -> acc.Add(l, vp.unpickle r) ; aux()
                                | t -> invalidTok t

                            aux()

                        | t -> invalidTok t
                }

            | _ -> failwith "unsupported map type"

        member __.Field shape (HKT.Unpack fc) = 
            HKT.pack {
                label = shape.Label
                fpickle = fun k t -> fc.pickle k (shape.Get t)
                funpickle = fun r t -> shape.Set t (fc.unpickle r)
            }

        member __.Tuple shape (HKT.Unpacks fields) =
            HKT.pack {
                pickle = fun k t -> k StartArray; for f in fields do f.fpickle k t; k EndArray
                unpickle = fun r ->
                    match r true with
                    | StartArray ->
                        let mutable t = shape.CreateUninitialized()
                        for f in fields do t <- f.funpickle r t
                        t
                    | t -> invalidTok t
            }

        member __.Record shape (HKT.Unpacks fields) =
            let fieldDict = fields |> Seq.map (fun f -> f.label, f) |> dict
            HKT.pack {
                pickle = fun k record -> k StartObject; (for f in fields do k (Label f.label) ; f.fpickle k record); k EndObject
                unpickle = fun r ->
                    match r true with
                    | StartObject ->
                        let rec aux record =
                            match r true with
                            | Label l ->
                                let ok,p = fieldDict.TryGetValue l
                                if ok then p.funpickle r record
                                else aux record
                            | EndObject -> record
                            | t -> invalidTok t

                        aux (shape.CreateUninitialized())

                    | t -> invalidTok t
            }

        member __.Union shape (HKT.Unpackss fieldss) =
            let fieldDicts = fieldss |> Seq.map (Seq.map (fun f -> f.label, f) >> dict) |> Seq.toArray
            HKT.pack {
                pickle = fun k union -> 
                    k StartObject; 
                    let tag = shape.GetTag union
                    let case = shape.UnionCases.[tag]
                    k (Label "_case") ; k (String case.CaseInfo.Name)
                    for f in fieldss.[tag] do f.fpickle k union
                    k EndObject

                unpickle = fun r ->
                    match r true , r true , r true with
                    | StartObject, Label "_case", String caseName ->
                        let tag = shape.GetTag caseName
                        let case = shape.UnionCases.[tag]
                        let fieldDict = fieldDicts.[tag]
                        let rec aux union =
                            match r true with
                            | Label l ->
                                let ok,p = fieldDict.TryGetValue l
                                if ok then p.funpickle r union
                                else aux union
                            | EndObject -> union
                            | t -> invalidTok t

                        aux (case.CreateUninitialized())

                    | t -> invalidTok t
            }

        member __.CliMutable shape (HKT.Unpacks fields) =
            let fieldDict = fields |> Seq.map (fun f -> f.label, f) |> dict
            HKT.pack {
                pickle = fun k record -> k StartObject; (for f in fields do k (Label f.label) ; f.fpickle k record); k EndObject
                unpickle = fun r ->
                    match r true with
                    | StartObject ->
                        let rec aux record =
                            match r true with
                            | Label l ->
                                let ok,p = fieldDict.TryGetValue l
                                if ok then p.funpickle r record
                                else aux record
                            | EndObject -> record
                            | t -> invalidTok t

                        aux (shape.CreateUninitialized())

                    | t -> invalidTok t
            }

        member __.Delay cell =
            HKT.pack { 
                pickle = fun k t -> (HKT.unpack cell.Value).pickle k t ; 
                unpickle = fun r -> (HKT.unpack cell.Value).unpickle r 
            }

let mkPickler<'t> () : Pickler<'t> = TypeBuilder.fold (PickerBuilder()) |> HKT.unpack 

let pickle (t : 't) = 
    let p = mkPickler<'t>()
    let acc = ResizeArray()
    p.pickle acc.Add t
    acc.ToArray()

let unpickle<'t> (tokens : seq<Token>) =
    let p = mkPickler<'t>()
    let tokens = Seq.toArray tokens
    let mutable i = 0
    p.unpickle (fun advance -> let t = tokens.[i] in (if advance then i <- i + 1); t)

let roundTrip<'t> (t : 't) = pickle t |> unpickle<'t>

//----------------------

type P = Z | S of P

pickle [1;2;3] // val it : Token [] = [|StartArray; Number 1M; Number 2M; Number 3M; EndArray|]
pickle [Some 42; None; Some 1] // [|StartArray; Number 42M; Null; Number 1M; EndArray|]
pickle (42, Some "42", {| x = 2 ; y = S(S(S Z)) |})
//val it : Token [] =
//  [|StartArray; Number 42M; EndArray; String "42"; EndArray; StartObject;
//    Label "x"; Number 2M; Label "y"; StartObject; Label "_case"; String "S";
//    StartObject; Label "_case"; String "S"; StartObject; Label "_case";
//    String "S"; StartObject; Label "_case"; String "Z"; EndObject; EndObject;
//    EndObject; EndObject; EndObject; EndArray|]