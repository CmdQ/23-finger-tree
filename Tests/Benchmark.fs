module CmdQ.FingerTree.Tests.Benchmark

open CmdQ.FingerTree
open PerfUtil
open System
open System.Diagnostics

[<AbstractClass>]
type Operation(name) =
    interface ITestable with
        member __.Name = name
        member me.Fini () = me.Fini()
        member me.Init () = me.Init()

    abstract Init : unit -> unit
    default __.Init () = ()

    abstract Fini : unit -> unit
    default __.Fini () = ()

    abstract Run : unit -> unit

let timedRun (benchmark:PerformanceTester<#Operation>) header repeat =
    let sw = Stopwatch.StartNew()
    benchmark.Run((fun b -> b.Run()), header, repeat)
    sw.Stop()
    let s = double sw.ElapsedMilliseconds / 1000.0
    printfn "\t%.3f s total (%i repetitions with %.3f s each)" s repeat (s / float repeat)

module InsertAppendOrDelete =
    [<AbstractClass>]
    type Benchmark(size, name) =
        inherit Operation(name)

        let rand = Random 23

        abstract member PopRight : unit -> unit
        abstract member Prepend : int -> unit
        abstract member Append : int -> unit

        override me.Run () =
            for i = 1 to size do
                let num = rand.Next()
                if num % 3 = 0 then
                    me.Prepend num
                elif num % 3 = 1 && size > 0 then
                    me.PopRight()
                else
                    me.Append num

    [<Sealed>]
    type BConcatDeque(size) =
        inherit Benchmark(size, "ConcatDeque")

        let mutable data = ConcatDeque.empty
        let mutable size = 0

        override __.PopRight () =
            if not <| ConcatDeque.isEmpty data then
                data <- data |> ConcatDeque.tail
                size <- size - 1

        override __.Append num =
            data <- data |> ConcatDeque.append num
            size <- size + 1

        override __.Prepend num =
            data <- data |> ConcatDeque.prepend num
            size <- size + 1

        override __.Fini () =
            let a = data |> ConcatDeque.toArray
            ignore a

    [<Sealed>]
    type BResizeArray(size) =
        inherit Benchmark(size, "ResizeArray")

        let mutable data = ResizeArray()

        override __.Init () =
            base.Init()
            data <- ResizeArray()

        override __.PopRight () =
            if data.Count > 0 then
                data.RemoveAt(data.Count - 1)

        override __.Append num =
            data.Add num

        override __.Prepend num =
            data.Insert(0, num)

    let titler = sprintf "InsertAppendOrDelete %s"

    let small = let size = 999 in ImplementationComparer<Benchmark>(BConcatDeque(size), [BResizeArray(size)], warmup = true)
    let large = let size = 99999 in ImplementationComparer<Benchmark>(BConcatDeque(size), [BResizeArray(size)], warmup = true)

    let compareSmall () = timedRun small (titler "small") 700
    let compareLarge () = timedRun large (titler "large") 7

module Divisors =
    let factors x =
        let rec factors acc d =
            if d > 1 then
                d - 1 |> factors (if x % d = 0 then d::acc else acc)
            else
                1::acc
        x / 2 |> factors [x]

    [<AbstractClass>]
    type Benchmark<'a>(size, name) =
        inherit Operation(name)

        let divisors =
            Array.init size id
            |> Array.Parallel.map factors

        abstract FromList : int list -> 'a
        abstract Collect<'b> : ('b -> 'a) -> seq<'b> -> 'a
        abstract Concat : 'a -> unit

        override me.Run () =
            let toAdd =
                divisors
                |> Array.Parallel.map me.FromList
                |> me.Collect id
            me.Concat toAdd

    [<Sealed>]
    type BConcatDeque(size) =
        inherit Benchmark<FingerTree<int>>(size, "ConcatDeque")

        let mutable data = ConcatDeque.empty

        override __.FromList x =
            ConcatDeque.ofList x

        override __.Concat x =
            data <- ConcatDeque.concat data x

        override __.Collect f x =
            x |> ConcatDeque.collect f

    [<Sealed>]
    type BResizeArray(size) =
        inherit Benchmark<ResizeArray<int>>(size, "ResizeArray")

        let data = ResizeArray()

        override __.FromList x = ResizeArray x

        override __.Concat x =
            data.AddRange x

        override __.Collect f x =
            x |> Seq.collect f |> ResizeArray

    let titler = sprintf "Divisors %s"

    let small = let size = 4096 in ImplementationComparer<Operation>(BConcatDeque(size), [BResizeArray(size)], warmup = true)
    let large = let size = 65536 in ImplementationComparer<Operation>(BConcatDeque(size), [BResizeArray(size)], warmup = true)
    let compareSmall () = timedRun small (titler "small") 300
    let compareLarge () = timedRun large (titler "large") 10

module ConstructFromArray =
    let name = "ConstructFromArray"
    let file = name + ".xml"

    type Benchmark(size) =
        inherit Operation("ofArray")

        override __.Run () =
            for l in [1000..2000..size] do
                Array.zeroCreate l
                |> ConcatDeque.ofArray
                |> ignore

    let test =
        let bench = Benchmark 100000
        PastImplementationComparer<Benchmark>(bench, Version(1, 1), warmup = true, historyFile = file)

    let run () =
        timedRun test name 10
        test.PersistCurrentResults file

module DeconstructTree =
    let name = "DeconstructTree"
    let file = name + ".xml"

    [<AbstractClass>]
    type Benchmark(name) =
        inherit Operation(name)

        static member Tree = seq { 1 .. 10000 } |> ConcatDeque.ofSeq

    type List() =
        inherit Benchmark("toList")

        override __.Run () =
            Benchmark.Tree |> ConcatDeque.toList |> ignore

    type Array() =
        inherit Benchmark("toArray")

        override __.Run () =
            Benchmark.Tree |> ConcatDeque.toArray |> ignore

    let versus = ImplementationComparer<Benchmark>(Array(), [List()], warmup = true)
    let compare () = timedRun versus "DeconstructTree" 600

    let listAlone = PastImplementationComparer<List>(List(), Version(1, 0), warmup = true, historyFile = file)
    let history () =
        timedRun listAlone "DeconstructTree" 700
        listAlone.PersistCurrentResults file

let benchmarks = [
    InsertAppendOrDelete.compareSmall
    InsertAppendOrDelete.compareLarge
    Divisors.compareSmall
    Divisors.compareLarge
    ConstructFromArray.run
    DeconstructTree.compare
    DeconstructTree.history
]
