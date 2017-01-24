#load "empty.fsx"

open System
open Empty

/// ksprintf with optional evaluatoin
let ksprintf (kontOpt : (string -> 'T) option) fmt =
    match kontOpt with
    | None -> empty
    | Some kont -> Printf.ksprintf (Some << kont) fmt

ksprintf (Some System.Console.WriteLine) "%s=%d" "key" 42

#time "on"

// formatting off
// Real: 00:00:00.161, CPU: 00:00:00.156, GC gen0: 99, gen1: 0, gen2: 0
for i = 1 to 1000000 do
    ksprintf None "Foo=%s, Bar=%d, Baz=%O" "foo" 42 System.ConsoleColor.Red |> ignore

// formatting on
// Real: 00:00:01.733, CPU: 00:00:01.750, GC gen0: 316, gen1: 0, gen2: 0
for i = 1 to 1000000 do
    ksprintf (Some ignore) "Foo=%s, Bar=%d, Baz=%O" "foo" 42 System.ConsoleColor.Red |> ignore

let value = ([1..5],Some "42")

// formatting off
// Real: 00:00:00.101, CPU: 00:00:00.109, GC gen0: 80, gen1: 0, gen2: 0
for i = 1 to 1000000 do
    ksprintf None "%A" value |> ignore

// formatting on
// Real: 00:07:14.970, CPU: 00:07:11.796, GC gen0: 28445, gen1: 43, gen2: 4
for i = 1 to 1000000 do
    ksprintf (Some ignore) "%A" value |> ignore