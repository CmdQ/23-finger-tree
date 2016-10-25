open CmdQ.Tests
open Fuchu
open FsCheck

[<EntryPoint>]
let main argv =
    let minTime = 3000L

    Arb.registerByType typeof<MyArbitraries.PosInt>.DeclaringType |> ignore
    let re = Tests.defaultMainThisAssembly argv
    if Array.contains "--benchmark" argv then
        printfn "Benchmarking..."
        Benchmark.runVersusDisplay minTime Benchmark.benchmarks
    re