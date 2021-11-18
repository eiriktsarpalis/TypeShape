namespace TypeShape.Core

// Extensibility for types exclusive to the net6.0 TFM

open System
open TypeShape.Core
open TypeShape.HKT

module Shape =

    let (|DateOnly|_|) (shape : TypeShape) =
        match shape with
        | :? TypeShape<DateOnly> -> Some ()
        | _ -> None

    let (|TimeOnly|_|) (shape : TypeShape) =
        match shape with
        | :? TypeShape<DateOnly> -> Some ()
        | _ -> None

type IDateOnlyBuilder<'F> =
    abstract DateOnly : unit -> App<'F, DateOnly>

type ITimeOnlyBuilder<'F> =
    abstract TimeOnly : unit -> App<'F, TimeOnly>

module Fold =

    let private unwrap (x : App<'F,_> ) : App<'F,_> = unbox x

    let (|DateOnly|_|) (builder : IDateOnlyBuilder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.DateOnly -> builder.DateOnly() |> unwrap |> Some
        | _ -> None

    let (|TimeOnly|_|) (builder : ITimeOnlyBuilder<'F>) (shape : TypeShape<'t>) : App<'F, 't> option =
        match shape with
        | Shape.DateOnly -> builder.TimeOnly() |> unwrap |> Some
        | _ -> None