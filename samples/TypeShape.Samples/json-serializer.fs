module JsonSerializer

open System
open System.Text
open System.Text.RegularExpressions
open FParsec
open TypeShape.Core
open TypeShape.Core.Utils

// Toy Json Serializer for F# values adapted from 
// http://www.quanttec.com/fparsec/tutorial.html#parsing-json

type Parser<'T> = Parser<'T, unit>

type JsonPickler<'T> =
    {
        Printer : StringBuilder -> 'T -> unit
        Parser : Parser<'T>
    }

[<AutoOpen>]
module PrinterImpl =
    
    let [<Literal>] nullStr = "null"

    let inline append (sb : StringBuilder) (t : string) = sb.Append t |> ignore

    let private escapeChars = Regex("[\n\r\"]", RegexOptions.Compiled)
    let private matchev = 
        MatchEvaluator(fun m -> 
            match m.Value with 
            | "\n" -> "\\n" 
            | "\r" -> "\\r" 
            | "\"" -> "\\\"" 
            | v -> v)

    let escapeStr (text:string) =
        let escaped = escapeChars.Replace(text, matchev)
        "\"" + escaped + "\""

    let printField (sb : StringBuilder) (label : string) printer value =
        append sb (escapeStr label)
        append sb " : "
        printer sb value

[<AutoOpen>]
module ParserImpl =
    let spaced p = between spaces spaces p

    let inline delay (f : unit -> 'T) : Parser<'T> =
        fun _ -> Reply(f())

    let (<*>) (f : Parser<'T -> 'S>) (t : Parser<'T>) : Parser<'S> = 
        fun stream ->
            let tr = t stream
            if tr.Status <> Ok then Reply(tr.Status, tr.Error) else
            let fr = f stream
            if fr.Status <> Ok then Reply(tr.Status, tr.Error) else
            Reply(fr.Result tr.Result)

    let nullLiteral t : Parser<'T> = stringReturn "null" t
    let boolLiteral : Parser<bool> =
        (stringReturn "true"  true) <|> (stringReturn "false" false)

    let numLiteral : Parser<float> = pfloat

    let stringLiteral : Parser<string> =
        let escape =  anyOf "\"\\/bfnrt"
                      |>> function
                          | 'b' -> "\b"
                          | 'f' -> "\u000C"
                          | 'n' -> "\n"
                          | 'r' -> "\r"
                          | 't' -> "\t"
                          | c   -> string c // every other char is mapped to itself

        let unicodeEscape =
            /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
            let hex2int c = (int c &&& 15) + (int c >>> 6)*9

            pchar 'u' >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                |> char |> string
            )

        let escapedCharSnippet = pchar '\\' >>. (escape <|> unicodeEscape)
        let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')

        between (pchar '\"') (pchar '\"')
                (stringsSepBy normalCharSnippet escapedCharSnippet)

    let jsonArray parser =
        between (pchar '[') (pchar ']') (sepBy (spaced parser) (pchar ','))

    let jsonObj parser =
        between (pchar '{') (pchar '}') (spaced parser)

    let jsonField label parser = spaced (pstring (escapeStr label)) >>. pchar ':' >>. spaced parser


/// Generates a json pickler for supplied type
let rec genPickler<'T> () : JsonPickler<'T> =
    let ctx = new TypeGenerationContext()
    genPicklerCached<'T> ctx
    
and private genPicklerCached<'T> (ctx : TypeGenerationContext) : JsonPickler<'T> =
    // create a delayed uninitialized instance for recursive type definitions
    let delay (c : Cell<JsonPickler<'T>>) : JsonPickler<'T> =
        { Parser = fun s -> c.Value.Parser s ;
          Printer = fun sb -> c.Value.Printer sb }

    match ctx.InitOrGetCachedValue<JsonPickler<'T>> delay with
    | Cached(value = f) -> f
    | NotCached t ->
        let p = genPicklerAux<'T> ctx
        ctx.Commit t p
    
and private genPicklerAux<'T> (ctx : TypeGenerationContext) : JsonPickler<'T> =
    let mkPickler
        (printer : StringBuilder -> 'a -> unit)
        (parser : Parser<'a>) : JsonPickler<'T> =
        { Printer = unbox printer ; Parser = spaced(unbox parser) }

    let mkMemberPickler (shape : IShapeWriteMember<'Class>) =
        shape.Accept { new IWriteMemberVisitor<'Class, (StringBuilder -> 'Class -> unit) * Parser<'Class -> 'Class>> with

            member __.Visit (shape : ShapeWriteMember<'Class, 'Field>) =
                let fP = genPicklerCached<'Field> ctx
                let printer sb c = 
                    let field = shape.Project c
                    printField sb shape.Label fP.Printer field

                let parser = jsonField shape.Label fP.Parser |>> fun f c -> shape.Inject c f
                printer, parser
        }

    let combineMemberPicklers (init : Parser<'Class>) (members : IShapeWriteMember<'Class> []) =
        let printers, parsers = members |> Array.map mkMemberPickler |> Array.unzip
        let printer sb (c : 'Class) =
            for i = 0 to members.Length - 1 do
                if i > 0 then append sb ", "
                printers.[i] sb c

        let parser =
            match Array.toList parsers with
            | [] -> init
            | hd :: tl -> List.fold (fun acc p -> (pchar ',' >>. p) <*> acc) (hd <*> init) tl

        mkPickler printer parser

    let withObjBrackets (p : JsonPickler<'T>) =
        let pr = p.Printer
        { Parser = jsonObj p.Parser ; Printer = fun sb t -> append sb "{ " ; pr sb t ; append sb " }" }

    match shapeof<'T> with
    | Shape.Unit -> mkPickler (fun sb _ -> append sb nullStr) (nullLiteral ())
    | Shape.Bool -> mkPickler (fun sb b -> append sb (string b)) boolLiteral
    | Shape.Byte -> mkPickler (fun sb b -> append sb (b |> float |> string)) (numLiteral |>> byte)
    | Shape.Int32 -> mkPickler (fun sb i -> append sb (string i)) (numLiteral |>> int)
    | Shape.Int64 -> mkPickler (fun sb i -> append sb (string i)) (numLiteral |>> int64)
    | Shape.String -> mkPickler (fun sb str -> append sb (escapeStr str)) stringLiteral
    | Shape.FSharpOption s ->
        s.Accept {
            new IFSharpOptionVisitor<JsonPickler<'T>> with
                member __.Visit<'t> () =
                    let tP = genPicklerCached<'t> ctx
                    let printer (sb : StringBuilder) (inp : 't option) =
                        match inp with
                        | None -> append sb nullStr
                        | Some t -> tP.Printer sb t

                    let nP = nullLiteral None
                    let sP = tP.Parser |>> Some
                    let parser = nP <|> sP
                    mkPickler printer parser
        }

    | Shape.FSharpList s ->
        s.Accept {
            new IFSharpListVisitor<JsonPickler<'T>> with
                member __.Visit<'t> () =
                    let eP = genPicklerCached<'t> ctx
                    let printer sb (ts : 't list) =
                        append sb "["
                        match ts with
                        | [] -> ()
                        | hd :: tl ->
                            eP.Printer sb hd
                            for t in tl do 
                                append sb ", "
                                eP.Printer sb t

                        append sb "]"

                    let parser = jsonArray eP.Parser

                    mkPickler printer parser
        }

    | Shape.Array s when s.Rank = 1 ->
        s.Accept {
            new IArrayVisitor<JsonPickler<'T>> with
                member __.Visit<'t> _ =
                    let eP = genPicklerCached<'t> ctx
                    let printer sb (ts : 't []) =
                        append sb "["
                        if ts.Length > 0 then
                            eP.Printer sb ts.[0]
                            for i in 1 .. ts.Length - 1 do
                                append sb ", "
                                eP.Printer sb ts.[i]

                        append sb "]"

                    let parser = jsonArray eP.Parser |>> Array.ofList

                    mkPickler printer parser
        }

    | Shape.FSharpMap s ->
        s.Accept {
            new IFSharpMapVisitor<JsonPickler<'T>> with
                member __.Visit<'k,'v when 'k : comparison> () =
                    if typeof<'k> <> typeof<string> then failwithf "Type '%O' is not supported" typeof<'T>
                    let vp = genPicklerCached<'v> ctx
                    let printer sb (m : Map<string, 'v>) =
                        append sb "{ "
                        let mutable first = true
                        for kv in m do
                            if first then first <- false else append sb ", "
                            append sb (escapeStr kv.Key)
                            append sb " : "
                            vp.Printer sb kv.Value

                        append sb " }"

                    let parser =
                        let keyValue = stringLiteral .>> spaced (pchar ':') .>>. vp.Parser
                        sepBy keyValue (spaced (pchar ',')) |>> Map.ofList |> jsonObj

                    mkPickler printer parser
        }

    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
        combineMemberPicklers (delay shape.CreateUninitialized) shape.Elements
        |> withObjBrackets

    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
        combineMemberPicklers (delay shape.CreateUninitialized) shape.Fields
        |> withObjBrackets

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
        let mkUnionCaseInfo (case : ShapeFSharpUnionCase<'T>) =
            let hasFields = case.Fields.Length > 0
            let pickler = combineMemberPicklers (delay case.CreateUninitialized) case.Fields
            let parser = if hasFields then spaced (pchar ',') >>. pickler.Parser else pickler.Parser
            let printer sb t = (if hasFields then append sb ", ") ; pickler.Printer sb t
            escapeStr case.CaseInfo.Name, printer, parser

        let caseInfo = shape.UnionCases |> Array.map mkUnionCaseInfo

        {
            Printer = 
                fun (sb:StringBuilder) (t:'T) ->
                    let tag = shape.GetTag t
                    let label, printer, _ = caseInfo.[tag]
                    printField sb "__case" append label
                    printer sb t

            Parser =
                jsonField "__case" stringLiteral >>= 
                    fun tag -> 
                        let t = shape.GetTag tag 
                        let _,_,parser = caseInfo.[t]
                        parser

        } |> withObjBrackets

    | _ -> failwithf "unsupported type '%O'" typeof<'T>

//-----------------------------------
// Serialization functions

let serialize (pickler : JsonPickler<'T>) (value : 'T) : string =
    let sb = new StringBuilder()
    pickler.Printer sb value
    sb.ToString()

let deserialize (pickler : JsonPickler<'T>) (json : string) : 'T =
    match run pickler.Parser json with
    | Success(r,_,_) -> r
    | Failure(msg,_,_) -> failwithf "Parse error: %s" msg


//--------------------------
// Examples

let p1 = genPickler<int * int list>()

serialize p1 (1, [2;3;4])
|> deserialize p1

let p2 = genPickler<int * string list option * string ref>()

serialize p2 (1, Some(["a";"b";"c"]), ref "foo")
|> deserialize p2

type Foo = { A : int ; B : string }

type Bar =
    | Foo of Foo
    | Bar of int
    | C
    | D of string option
    | E of Map<string, int>

let p3 = genPickler<Bar list []>()

serialize p3 [| [] ; [C ; D None] ; [Foo { A = 42 ; B = "12" }] ; [E (Map.ofList [("foo", 42)])]|]
|> deserialize p3

// Recursive type serialization

type BinTree<'T> = Leaf | Node of 'T * BinTree<'T> * BinTree<'T>

let p4 = genPickler<BinTree<int>> ()

serialize p4 (Node(3, Node(1, Leaf, Node(2, Leaf,Leaf)), Leaf))
|> deserialize p4