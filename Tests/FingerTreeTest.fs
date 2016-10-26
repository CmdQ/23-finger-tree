module CmdQ.Tests.FingerTreeTest

open MyArbitraries
open CmdQ
open Fuchu
open FsCheck

/// Testing extensions for finger tree module.
module Finger =
    /// Regular finger tree doesn't provide a length, so we work around.
    let length tree = tree |> FingerTree.toSeq |> Seq.length

[<Tests>]
let properties =
    testList "Basic finger tree" [
        testProperty "Empty finger tree, one insertion" <|
            fun elm ->
                let ft = FingerTree.empty
                Finger.length ft = 0
             && (ft |> FingerTree.append elm |> Finger.length) = 1
             && (ft |> FingerTree.prepend elm |> Finger.length) = 1

        testProperty "Appending increases size" <|
            fun coll elm1 elm2 ->
                let ft = coll |> FingerTree.ofArray
                let before = Finger.length ft
                let bigger = ft |> FingerTree.append elm1
                let after = Finger.length bigger
                let evenBigger = bigger |> FingerTree.prepend elm2
                after = before + 1 && Finger.length evenBigger = after + 1

        testProperty "Tree of array must go back to array" <|
            fun array ->
                let ft = array |> FingerTree.ofArray
                ft |> FingerTree.toArray = array

        testProperty "Tree of list must go back to list" <|
            fun list ->
                let ft = list |> FingerTree.ofList
                ft |> FingerTree.toList = list

        testProperty "Prepended items must show up at the front" <|
            fun (list:PosInt list) (NegInt elm) ->
                (List.length list > 0) ==> lazy (
                        // Remove enclosing type.
                        let list = List.map int list

                        let ft = list |> FingerTree.ofList
                        let added = ft |> FingerTree.prepend elm

                        // Make sure, no negatives are in there...
                        list |> List.forall (flip (>=) 0)
                        // ... and that our candidate is negative.
                     && elm < 0
                     && List.length list > 0
                     && Finger.length ft > 0
                     && ft |> FingerTree.head <> elm
                     && added |> FingerTree.head = elm
                    )

        testProperty "Appended items must show up at the end" <|
            fun (list:PosInt list) (NegInt elm) ->
                (List.length list > 0) ==> lazy (
                        // Remove enclosing type.
                        let list = List.map int list

                        let ft = list |> FingerTree.ofList
                        let added = ft |> FingerTree.append elm

                        // Make sure, no negatives are in there...
                        list |> List.forall (flip (>=) 0)
                        // ... and that our candidate is negative.
                     && elm < 0
                     && List.length list > 0
                     && Finger.length ft > 0
                     && ft |> FingerTree.last <> elm
                     && added |> FingerTree.last = elm
                    )

        testProperty "Concatenating adds two lengths" <|
            fun list1 list2 ->
                let c1 = List.length list1
                let c2 = List.length list2
                let tree1 = list1 |> FingerTree.ofList<string>
                let tree2 = list2 |> FingerTree.ofList
                let concat = FingerTree.concat tree1 tree2
                Finger.length concat = c1 + c2

        testProperty "Concatenation is the same as n appends" <|
            fun left right ->
                let tree = left |> FingerTree.ofList<char>
                let concat = right |> FingerTree.ofList |> FingerTree.concat tree
                let alterative = right |> List.fold (flip FingerTree.append) tree
                FingerTree.toList concat = FingerTree.toList alterative

        testProperty "Reversed concatenation is the same as n prepends" <|
            fun left right ->
                let tree = right |> FingerTree.ofList<decimal>
                let concat = FingerTree.concat (left |> FingerTree.ofList) tree
                let alterative = tree |> List.foldBack FingerTree.prepend left
                FingerTree.toList concat = FingerTree.toList alterative
    ]
