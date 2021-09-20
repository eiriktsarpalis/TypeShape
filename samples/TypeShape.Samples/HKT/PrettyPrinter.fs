module TypeShape.HKT.PrettyPrinter

open System
open System.Text
open TypeShape.HKT

module Impl =

    // Internal Pretty-Printer implementation for type 'a
    // Action<_,_> is faster than F# lambdas
    type PP<'a> = Action<StringBuilder, 'a>

    // HKT encoding for PrettyPrinter
    type PrettyPrinter =
        static member Assign(_ : App<PrettyPrinter, 'a>, _ : PP<'a>) = ()

    let inline append (sb : StringBuilder) (value : string) = sb.Append(value) |> ignore
    let inline appendSeq (sb : StringBuilder) xs (f : Action<StringBuilder, 'a>) =
        let mutable first = true
        for x in xs do
            if first then first <- false else sb.Append "; " |> ignore
            f.Invoke(sb, x)

    let prettyPrinterBuilder =
        { new ITypeBuilder<PrettyPrinter, PrettyPrinter> with
            member _.Bool () = HKT.pack(PP(fun s b -> append s (if b then "true" else "false")))
            member _.Byte () = HKT.pack(PP(fun s b -> s.Append(b).Append("uy") |> ignore))
            member _.SByte() = HKT.pack(PP(fun s b -> s.Append(b).Append("y") |> ignore))
            member _.Char () = HKT.pack(PP(fun s c -> s.Append('\'').Append(c).Append('\'') |> ignore))
        
            member _.Int16 () = HKT.pack(PP(fun s i -> s.Append(i).Append('s') |> ignore))
            member _.Int32 () = HKT.pack(PP(fun s i -> s.Append(i) |> ignore))
            member _.Int64 () = HKT.pack(PP(fun s i -> s.Append(i).Append('L') |> ignore))

            member _.UInt16 () = HKT.pack(PP(fun s i -> s.Append(i).Append("us") |> ignore))
            member _.UInt32 () = HKT.pack(PP(fun s i -> s.Append(i).Append('u' ) |> ignore))
            member _.UInt64 () = HKT.pack(PP(fun s i -> s.Append(i).Append("uL") |> ignore))

            member _.Single () = HKT.pack(PP(fun s f -> s.Append(f).Append('f') |> ignore))
            member _.Double () = HKT.pack(PP(fun s f -> s.Append(f) |> ignore))
            member _.Decimal() = HKT.pack(PP(fun s f -> s.Append(f).Append('M') |> ignore))
            member _.BigInt () = HKT.pack(PP(fun s f -> s.Append(f).Append('I') |> ignore))

            member _.Unit() = HKT.pack(PP(fun s () -> append s "()"))
            member _.String () = HKT.pack(PP(fun s str -> s.Append('"').Append(str).Append('"') |> ignore))
            member _.Guid () = HKT.pack(PP(fun s g -> s.Append(g) |> ignore))

            member _.TimeSpan () = HKT.pack(PP(fun s t -> s.Append(t) |> ignore))
            member _.DateTime () = HKT.pack(PP(fun s d -> s.Append(d) |> ignore))
            member _.DateTimeOffset() = HKT.pack(PP(fun s d -> s.Append(d) |> ignore))

            member _.Nullable (HKT.Unpack ep) = HKT.pack(PP(fun s x -> if x.HasValue then ep.Invoke(s, x.Value) else append s "null"))
            member _.Enum _ = HKT.pack(PP(fun s e -> s.Append e |> ignore))
            member _.Array (HKT.Unpack ep) = HKT.pack(PP(fun s xs -> append s "[|" ; appendSeq s xs ep ; append s "|]"))

            member _.Option (HKT.Unpack ep) = HKT.pack(PP(fun s x -> match x with None -> append s "None" | Some x -> append s "Some(" ; ep.Invoke(s, x) ; append s ")"))
            member _.Ref (HKT.Unpack ep) = HKT.pack(PP(fun s x -> append s "{ contents = "; ep.Invoke(s, x.Value) ; append s " } "))
            member _.List (HKT.Unpack ep) = HKT.pack(PP(fun s xs -> append s "[" ; appendSeq s xs ep ; append s "]"))
            member _.Set (HKT.Unpack ep) = HKT.pack(PP(fun s xs -> append s "set [" ; appendSeq s xs ep ; append s "]"))
            member _.Map (HKT.Unpack kp) (HKT.Unpack vp) = 
                HKT.pack(PP(fun s xs -> 
                    let mutable first = true
                    append s "map ["
                    for kv in xs do
                        if first then first <- false else append s "; "

                        append s "("
                        kp.Invoke(s, kv.Key)
                        append s ", "
                        vp.Invoke(s, kv.Value)
                        append s ")"
                    append s "]"))

            member _.Field shape (HKT.Unpack fp) = HKT.pack(PP(fun s t -> fp.Invoke(s, shape.GetByRef &t)))

            member _.Tuple shape (HKT.Unpacks fields) =
                let isStruct = shape.IsStructTuple
                HKT.pack(PP(fun s tuple ->
                    if isStruct then append s "struct(" else append s "("
                    let mutable isFirst = true
                    for f in fields do
                        if isFirst then isFirst <- false else append s ", "
                        f.Invoke(s, tuple)
                    append s ")"))

            member _.Record shape (HKT.Unpacks fields) =
                let isAnon = shape.IsAnonymousRecord
                let isStruct = shape.IsStructRecord
                let fieldLabels = 
                    shape.Fields 
                    |> Array.map (fun f -> f.Label)
                    |> Array.zip fields

                HKT.pack(PP(fun s record ->
                    if isAnon then 
                        if isStruct 
                        then append s "struct {|"
                        else append s "{|"
                    else append s "{"

                    let mutable isFirst = true
                    for field,label in fieldLabels  do
                        if isFirst then isFirst <- false else append s "; "
                        append s label
                        append s " = "
                        field.Invoke(s, record)
               
                    if isAnon then append s "|}" else append s "}"))

            member _.Union shape (HKT.Unpackss fieldss) =
                HKT.pack(PP(fun s union ->
                    let tag = shape.GetTagByRef &union
                    let case = shape.UnionCases.[tag]
                    append s case.CaseInfo.Name
                    if case.Fields.Length > 0 then
                        append s "("
                        let mutable first = true
                        for f in fieldss.[tag] do
                            if first then first <- false else append s ", "
                            f.Invoke(s, union)
                        append s ")"))

            member _.CliMutable shape (HKT.Unpacks fields) =
                let name = shape.DefaultCtorInfo.DeclaringType.Name
                let fieldProps =
                    shape.Properties
                    |> Array.map (fun p -> p.Label)
                    |> Array.zip fields

                HKT.pack(PP(fun s record ->
                    append s name
                    append s "("
                    let mutable first = true
                    for f, label in fieldProps do
                        if first then first <- false else append s ", "
                        append s label
                        append s " = "
                        f.Invoke(s, record)
                    append s ")"))

            member _.Delay f = HKT.pack(PP(fun s x -> (HKT.unpack f.Value).Invoke(s, x))) }

/// builds printer function for given type
let mkPrinter<'t> () : 't -> string = 
    let action = TypeBuilder.fold Impl.prettyPrinterBuilder |> HKT.unpack
    fun t ->
        let sb = new StringBuilder()
        action.Invoke(sb, t)
        sb.ToString()

//-------------------------------------------------------
// Examples

#nowarn "20"

type P = Z | S of P

let p = mkPrinter<int * (string * DateTime) option * {| x : int ; p : P |}>()

p (42, Some ("42", DateTime.Now), {| x = 42 ; p = S(S(S Z)) |})