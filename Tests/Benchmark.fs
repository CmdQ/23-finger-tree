module CmdQ.Tests.Benchmark

open CmdQ
open System
open System.Diagnostics

[<AbstractClass>]
type Benchmark(name) =
    let mutable counter = 0

    abstract member Init:unit -> unit
    default __.Init () = counter <- 0

    abstract member OneStep:unit -> unit
    default __.OneStep () = counter <- counter + 1

    member __.Name = name

    member __.Count = counter

type Alternatives = {
    Benchmark:Benchmark
    Alternatives:Benchmark list
}

type Timing = {
    Name:string
    Time:float
    Ratio:float
}

type Comparison = {
    BaseTime:float
    Alternatives:Timing list
}

module InsertAppendOrDelete =
    let modder = 3

    type Finger() =
        inherit Benchmark("Insert or append with Finger") with
            let mutable rand = Random(23)
            let mutable data = CmdQ.FingerTree.empty
            let mutable size = 0

            override __.Init () =
                rand <- Random(23)
                data <- CmdQ.FingerTree.empty
                size <- 0

            override __.OneStep () =
                base.OneStep()
                let num = rand.Next()
                if num % modder = 0 then
                    data <- data |> FingerTree.prepend num
                    size <- size + 1
                elif num % modder = 1 && size > 0 then
                    data <- data |> FingerTree.tail
                    size <- size - 1
                else
                    data <- data |> FingerTree.append num
                    size <- size + 1

    type List() =
        inherit Benchmark("Insert or append with ResizeArray") with
            let mutable rand = Random(23)
            let mutable data = ResizeArray()

            override __.Init () =
                rand <- Random(23)
                data <- ResizeArray()

            override __.OneStep () =
                base.OneStep()
                let num = rand.Next()
                if num % modder = 0 then
                    data.Insert(0, num)
                elif num % modder = 1 && data.Count > 0 then
                    data.RemoveAt(data.Count - 1)
                else
                    data.Add num

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
        let mutable data = FingerTree.empty

        override __.Init () =
            stopped <- 0
            data <- FingerTree.empty

        override __.OneStep () =
            base.OneStep()
            let trials = Array.init arrayLength ((+)stopped)
            let divisors =
                trials
                |> Array.Parallel.map (factors >> FingerTree.ofList)
            let toAdd =
                divisors
                |> FingerTree.collect id
            data <- FingerTree.concat data toAdd

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

let run minTime (benchmark:Benchmark) =
    benchmark.Init()
    let rec run (sw:Stopwatch) =
        benchmark.OneStep()
        if sw.ElapsedMilliseconds < minTime || benchmark.Count = 0 then
            run sw
        else
            float sw.ElapsedMilliseconds / float benchmark.Count
    Stopwatch.StartNew() |> run

let runVersus minTime benchmarks =
    let runner = run minTime
    let baseTime = benchmarks.Benchmark |> runner
    let versus =
        benchmarks.Alternatives
        |> List.map (fun b ->
            let time = b |> runner
            { Name = b.Name; Time = time; Ratio = time / baseTime }
        )
    { BaseTime = baseTime; Alternatives = versus }

let displayRun minTime benchmarks =
    for b in benchmarks do
        let time = run minTime b
        printfn "%12f ms per iteration for %s." time b.Name

let runVersusDisplay minTime benchmarks =
    for b in benchmarks do
        printfn "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
        let times = runVersus minTime b
        printfn "%12f ms per iteration (%.2f) for %s.\nAlternatives:" times.BaseTime 1.0 b.Benchmark.Name
        for a in times.Alternatives do
            printfn "%12f ms per iteration (%.2f) for %s (%s)." a.Time a.Ratio a.Name (if a.Ratio < 1.0 then "faster" else "slower")
        printfn "----------------------------------------"

let benchmarks:Alternatives list = [
    { Benchmark = InsertAppendOrDelete.Finger(); Alternatives = [InsertAppendOrDelete.List()] }
    { Benchmark = Divisors.Finger(); Alternatives = [Divisors.Alternative()] }
]
