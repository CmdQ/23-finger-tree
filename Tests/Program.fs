open CmdQ.FingerTree.Tests
open Fuchu
open FsCheck

[<EntryPoint>]
let main argv =
    if Array.contains "--benchmark" argv then
        printfn "Benchmarking..."
        Benchmark.InsertAppendOrDelete.testBed.Run((fun b -> b.Run()), "InsertAppendOrDelete", repeat = 500)
        0
    else
        Arb.registerByType typeof<MyArbitraries.PosInt>.DeclaringType |> ignore
        Tests.defaultMainThisAssembly argv
