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

        testProperty "Check first and last element after sub" <|
            fun length start count ->
                let length = abs length
                let start = abs start
                let count = abs count
                let tree = Array.init length id |> RandomAccess.ofArray
                (count > 0 && start + count <= length)
                ==> lazy (
                    let sub = RandomAccess.sub tree start count
                    RandomAccess.get sub 0 = start && RandomAccess.get sub (count - 1) = count - 1 + start
                )
]
