open CmdQ.Tests
open Fuchu
open FsCheck

[<EntryPoint>]
let main argv =
    Arb.registerByType typeof<MyArbitraries.PosInt>.DeclaringType |> ignore
    Tests.defaultMainThisAssembly argv