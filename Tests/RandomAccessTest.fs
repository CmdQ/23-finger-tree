module CmdQ.FingerTree.Tests.RandomAccessTest

open CmdQ.FingerTree
open Fuchu
open FsCheck
open Swensen.Unquote

[<Tests>]
let empty =
    testCase "Size of empty" <|
        fun () ->
            RandomAccess.length RandomAccess.empty =! 0
            RandomAccess.isEmpty RandomAccess.empty =! true

[<Tests>]
let foo = fun () -> ()

[<Tests>]
let properties =
    testList "RandomAccess" [
        testProperty "Sizes match" <|
            fun input ->
                let tree:RandomAccess.Tree<bool> = RandomAccess.ofArray input
                RandomAccess.length tree = input.Length
                |> Prop.trivial (input.Length = 0)

        testProperty "Access by index" <|
            fun len ->
                let tree = Array.init (abs len) id |> RandomAccess.ofArray
                let back = tree |> RandomAccess.toArray
                let item = back |> Array.map (RandomAccess.get tree)
                back = item
]
