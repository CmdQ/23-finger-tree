open CmdQ.FingerTree.Tests
open Fuchu
open FsCheck

[<EntryPoint>]
let main argv =
    if Array.contains "--benchmark" argv then
        printfn "Benchmarking..."
        Benchmark.benchmarks |> List.iter (fun b -> b())
        0
    else
        printfn "Running tests..."
        Arb.registerByType typeof<MyArbitraries.PosInt>.DeclaringType |> ignore
        Tests.defaultMainThisAssembly argv
