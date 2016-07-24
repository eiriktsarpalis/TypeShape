#r "../../bin/TypeShape.dll"
open System
open TypeShape

let rec mkPrinter<'T> () : 'T -> string = mkPrinterUntyped typeof<'T> :?> _
and private mkPrinterUntyped (t : Type) : obj =
    match getShape t with
    | ShapeUnit -> box(fun () -> "()")
    | ShapeBool -> box(sprintf "%b")
    | ShapeInt32 -> box(sprintf "%d")
    | ShapeString -> box(sprintf "%s")
    | ShapeFSharpOption s ->
        s.Accept {
            new IFSharpOptionVisitor<obj> with
                member __.Visit<'T> () =
                    let tp = mkPrinter<'T>()
                    box(function None -> "None" | Some t -> sprintf "Some (%s)" (tp t))
        }

    | ShapeFSharpList s ->
        s.Accept {
            new IFSharpListVisitor<obj> with
                member __.Visit<'T> () =
                    let tp = mkPrinter<'T>()
                    box(fun ts -> ts |> List.map tp |> String.concat "; " |> sprintf "[%s]")
        }

    | ShapeTuple2 s ->
        s.Accept {
            new ITuple2Visitor<obj> with
                member __.Visit<'T, 'S> () =
                    let tp = mkPrinter<'T>()
                    let sp = mkPrinter<'S>()
                    box(fun (t : 'T, s : 'S) -> sprintf "(%s, %s)" (tp t) (sp s))
        }

    | _ -> failwithf "unsupported type '%O'" t


#time "on"

let p = mkPrinter<int list * (string option * (bool * unit))> ()
p ([1 .. 5], (None, (false, ())))
sprintf "%A" ([1 .. 5], (None, (false, ())))