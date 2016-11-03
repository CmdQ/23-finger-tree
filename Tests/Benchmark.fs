module CmdQ.FingerTree.Tests.Benchmark

open CmdQ.FingerTree
open PerfUtil
open System

[<AbstractClass>]
type Operation(name) =
    interface ITestable with
        member __.Name = name
        member __.Fini () = ()
        member __.Init () = ()
    abstract Run : unit -> unit

module InsertAppendOrDelete =
    [<AbstractClass>]
    type Benchmark(name) =
        inherit Operation(name)

        abstract member Step : unit -> unit

        abstract member Clear : unit -> unit

        override me.Run () =
            for i = 1 to 999 do
                me.Clear()
                me.Step()

    type ConcatDeque() =
        inherit Benchmark("ConcatDeque")

        let mutable rand = Random(23)
        let mutable data = ConcatDeque.empty
        let mutable size = 0

        override __.Clear () =
            rand <- Random(23)
            data <- ConcatDeque.empty
            size <- 0

        override __.Step () =
            let num = rand.Next()
            if num % 3 = 0 then
                data <- data |> ConcatDeque.prepend num
                size <- size + 1
            elif num % 3 = 1 && size > 0 then
                data <- data |> ConcatDeque.tail
                size <- size - 1
            else
                data <- data |> ConcatDeque.append num
                size <- size + 1

    type List() =
        inherit Benchmark("ResizeArray")

        let mutable rand = Random(23)
        let mutable data = ResizeArray()

        override __.Clear () =
            rand <- Random(23)
            data <- ResizeArray()

        override __.Step () =
            let num = rand.Next()
            if num % 3 = 0 then
                data.Insert(0, num)
            elif num % 3 = 1 && data.Count > 0 then
                data.RemoveAt(data.Count - 1)
            else
                data.Add num

    let testBed = ImplementationComparer<Benchmark>(ConcatDeque(), [List()])
(*
    type Finger() =
        inherit Benchmark("Insert or append with Finger") with

            interface ITestable with
                member __.Init () =
                    rand <- Random(23)
                    data <- ConcatDeque.empty
                    size <- 0

            override __.OneStep () =
                base.OneStep()


module Divisors =
    let arrayLength = 8192

    let factors x =
        let rec factors acc d =
            if d > 1 then
                d - 1 |> factors (if x % d = 0 then d::acc else acc)
            else
                1::acc
        x / 2 |> factors [x]

    type Finger() =
        inherit Benchmark("Divisors with Finger")

        let mutable stopped = 0
        let mutable data = ConcatDeque.empty

        override __.Init () =
            stopped <- 0
            data <- ConcatDeque.empty

        override __.OneStep () =
            base.OneStep()
            let trials = Array.init arrayLength ((+)stopped)
            let divisors =
                trials
                |> Array.Parallel.map (factors >> ConcatDeque.ofList)
            let toAdd =
                divisors
                |> ConcatDeque.collect id
            data <- ConcatDeque.concat data toAdd

    type Alternative() =
        inherit Benchmark("Divisors with ResizeArray")

        let mutable stopped = 0
        let mutable data = ResizeArray()

        override __.Init () =
            stopped <- 0
            data <- ResizeArray()

        override __.OneStep () =
            base.OneStep()
            let trials = Array.init arrayLength ((+)stopped)
            let divisors =
                trials
                |> Array.Parallel.map factors
            for d in divisors do
                data.AddRange d

                *)