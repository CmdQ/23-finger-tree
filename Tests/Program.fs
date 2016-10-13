open CmdQ.Tests
open Fuchu
open FsCheck

[<EntryPoint>]
let main argv =
    Arb.registerByType typeof<MyArbitraries.PosInt>.DeclaringType |> ignore
    let re = Tests.defaultMainThisAssembly argv
    if Array.contains "--benchmark" argv then
        run Benchmark.benchmarks |> ignore
    re