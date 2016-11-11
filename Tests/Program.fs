open CmdQ.FingerTree.Tests
open Fuchu
open FsCheck
open System.Diagnostics

[<EntryPoint>]
let main argv =
    if Array.contains "--benchmark" argv then
        printfn "Benchmarking..."
        let sw = Stopwatch.StartNew()
        Benchmark.benchmarks |> List.iter (fun b -> b())
        sw.Stop()
        printfn "\nBenchmarking took %.3f s in total." (float sw.ElapsedMilliseconds / 1000.0)
        0
    else
        printfn "Running tests..."
        Arb.registerByType typeof<MyArbitraries.PosInt>.DeclaringType |> ignore
        Tests.defaultMainThisAssembly argv
