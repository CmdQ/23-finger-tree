open Fuchu
open FsCheck

[<EntryPoint>]
let main argv =
    Arb.registerByType typeof<MyArbitraries.NegInt>.DeclaringType |> ignore
    Tests.defaultMainThisAssembly argv
