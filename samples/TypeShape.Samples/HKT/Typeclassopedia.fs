module Typeclassopedia

open TypeShape.HKT

// Common Haskell-inspired HKT applications

//------------------------
// Functor

[<AbstractClass>]
type Functor<'F when 'F :> Functor<'F>> () =
    abstract Fmap : ('a -> 'b) -> App<'F, 'a> -> App<'F, 'b>

let inline fmap (f : 'a -> 'b) (x : App<'F,'a>) : _ when 'F :> Functor<'F> = (new 'F()).Fmap f x

//------------------------
// Applicative

[<AbstractClass>]
type Applicative<'F when 'F :> Applicative<'F>> () =
    inherit Functor<'F>()

    abstract Pure : 'a -> App<'F, 'a>
    abstract Apply : App<'F, 'a -> 'b> -> App<'F, 'a> -> App<'F, 'b>

    override a.Fmap f fx = a.Apply (a.Pure f) fx

let inline puree (x : 'a) : _ when 'F :> Applicative<'F> = (new 'F()).Pure x
let inline (<*>) (f : App<'F, 'a -> 'b>) (fx : App<'F, 'a>) : _ when 'F :> Applicative<'F> = (new 'F()).Apply f fx

//------------------------
// Monad

[<AbstractClass>]
type Monad<'F when 'F :> Monad<'F>> () =
    inherit Applicative<'F>()

    abstract Return : 'a -> App<'F, 'a>
    abstract Bind : App<'F, 'a> * ('a -> App<'F, 'b>) -> App<'F, 'b>

    override m.Pure x = m.Return x
    override m.Apply f x = m { let! f' = f in let! x' = x in return f' x' }

let monad<'F when 'F :> Monad<'F> and 'F : (new : unit -> 'F)> = (new 'F())

//---------------------------------------
// Examples

let functorFlow fx =
    fx
    |> fmap (fun x -> x * x)
    |> fmap (fun x -> x + 1)

let appZip fx fy =
    puree (fun x y -> (x,y)) <*> fx <*> fy

let monadExpr x = monad {
    let! x = x
    let! y = monad { return 1 + 2 }
    return x * y
}

// Option monad

type Option() =
    inherit Monad<Option>()

    static member Assign(_ : App<Option, 'a>, _ : 'a option) = ()

    override __.Return x = HKT.pack (Some x)
    override __.Bind (HKT.Unpack x, f) = HKT.pack (Option.bind (f >> HKT.unpack) x)

let o1 = HKT.pack (Some 42) : App<Option, _>
let o2 = HKT.pack None : App<Option, string>

functorFlow o1 |> HKT.unpack
appZip o1 o1 |> HKT.unpack
appZip o1 o2 |> HKT.unpack
monadExpr o1 |> HKT.unpack

// List monad

type List() =
    inherit Monad<List>()

    static member Assign(_ : App<List, 'a>, _ : 'a list) = ()

    override __.Return x = HKT.pack [x]
    override __.Bind (HKT.Unpack xs, f) = HKT.pack (List.collect (f >> HKT.unpack) xs)

let l1 = HKT.pack [1 .. 10] : App<List, _>
let l2 = HKT.pack [false ; true] : App<List, _>

functorFlow l1 |> HKT.unpack
appZip l1 l2 |> HKT.unpack
monadExpr l1 |> HKT.unpack

// Async monad

type AsyncM() =
    inherit Monad<AsyncM>()

    static member Assign(_ : App<AsyncM, 'a>, _ : Async<'a>) = ()

    override __.Return x = HKT.pack (async.Return x)
    override __.Bind (HKT.Unpack xs, f) = HKT.pack (async.Bind(xs, f >> HKT.unpack))

let runM (x : App<AsyncM, _>) = Async.RunSynchronously(HKT.unpack x)

let a1 = HKT.pack (async { return 1 }) : App<AsyncM, _>
let a2 = HKT.pack (async { return false }) : App<AsyncM, _>

functorFlow a1 |> runM
appZip a1 a2 |> runM
monadExpr a1 |> runM