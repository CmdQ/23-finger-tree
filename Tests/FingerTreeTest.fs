module CmdQ.FingerTree.Tests.FingerTreeTest

open CmdQ.FingerTree
open MyArbitraries
open Fuchu
open FsCheck

/// Testing extensions for finger tree module.
module Finger =
    /// Regular finger tree doesn't provide a length, so we work around.
    let length tree = tree |> ConcatDeque.toSeq |> Seq.length

[<Tests>]
let properties =
    testList "Basic finger tree" [
        testProperty "Empty finger tree, one insertion" <|
            fun elm ->
                let ft = ConcatDeque.empty
                Finger.length ft = 0
             && (ft |> ConcatDeque.append elm |> Finger.length) = 1
             && (ft |> ConcatDeque.prepend elm |> Finger.length) = 1

        testProperty "Appending increases size" <|
            fun coll elm1 elm2 ->
                let ft = coll |> ConcatDeque.ofArray
                let before = Finger.length ft
                let bigger = ft |> ConcatDeque.append elm1
                let after = Finger.length bigger
                let evenBigger = bigger |> ConcatDeque.prepend elm2
                after = before + 1 && Finger.length evenBigger = after + 1

        testProperty "Tree of array must go back to array" <|
            fun array ->
                let ft = array |> ConcatDeque.ofArray
                ft |> ConcatDeque.toArray = array

        testProperty "Tree of list must go back to list" <|
            fun list ->
                let ft = list |> ConcatDeque.ofList
                ft |> ConcatDeque.toList = list

        testProperty "Prepended items must show up at the front" <|
            fun (list:PosInt list) (NegInt elm) ->
                (List.length list > 0) ==> lazy (
                        // Remove enclosing type.
                        let list = List.map int list

                        let ft = list |> ConcatDeque.ofList
                        let added = ft |> ConcatDeque.prepend elm

                        // Make sure, no negatives are in there...
                        list |> List.forall (flip (>=) 0)
                        // ... and that our candidate is negative.
                     && elm < 0
                     && List.length list > 0
                     && Finger.length ft > 0
                     && ft |> ConcatDeque.head <> elm
                     && added |> ConcatDeque.head = elm
                    )

        testProperty "Appended items must show up at the end" <|
            fun (list:PosInt list) (NegInt elm) ->
                (List.length list > 0) ==> lazy (
                        // Remove enclosing type.
                        let list = List.map int list

                        let ft = list |> ConcatDeque.ofList
                        let added = ft |> ConcatDeque.append elm

                        // Make sure, no negatives are in there...
                        list |> List.forall (flip (>=) 0)
                        // ... and that our candidate is negative.
                     && elm < 0
                     && List.length list > 0
                     && Finger.length ft > 0
                     && ft |> ConcatDeque.last <> elm
                     && added |> ConcatDeque.last = elm
                    )

        testProperty "Concatenating adds two lengths" <|
            fun list1 list2 ->
                let c1 = List.length list1
                let c2 = List.length list2
                let tree1:FingerTree<string> = list1 |> ConcatDeque.ofList
                let tree2 = list2 |> ConcatDeque.ofList
                let concat = ConcatDeque.concat tree1 tree2
                Finger.length concat = c1 + c2

        testProperty "Concatenation is the same as n appends" <|
            fun left right ->
                let tree:FingerTree<char> = left |> ConcatDeque.ofList
                let concat = right |> ConcatDeque.ofList |> ConcatDeque.concat tree
                let alterative = right |> List.fold (flip ConcatDeque.append) tree
                ConcatDeque.toList concat = ConcatDeque.toList alterative

        testProperty "Reversed concatenation is the same as n prepends" <|
            fun left right ->
                let tree:FingerTree<decimal> = right |> ConcatDeque.ofList
                let concat = ConcatDeque.concat (left |> ConcatDeque.ofList) tree
                let alterative = tree |> List.foldBack ConcatDeque.prepend left
                ConcatDeque.toList concat = ConcatDeque.toList alterative
    ]
