module CmdQ.Tests.FingerTreeTest

open MyArbitraries
open CmdQ
open Fuchu
open FsCheck

/// Testing extensions for finger tree module.
module Finger =
    /// Regular finger tree doesn't provide a length, so we work around.
    let length tree = tree |> Finger.toSeq |> Seq.length

[<Tests>]
let properties =
    testList "Basic finger tree" [
        testProperty "Empty finger tree, one insertion" <|
            fun elm ->
                let ft = Finger.empty
                Finger.length ft = 0
             && (ft |> Finger.append elm |> Finger.length) = 1
             && (ft |> Finger.prepend elm |> Finger.length) = 1

        testProperty "Appending increases size" <|
            fun coll elm1 elm2 ->
                let ft = coll |> Finger.ofArray
                let before = Finger.length ft
                let bigger = ft |> Finger.append elm1
                let after = Finger.length bigger
                let evenBigger = bigger |> Finger.prepend elm2
                after = before + 1 && Finger.length evenBigger = after + 1

        testProperty "Tree of array must go back to array" <|
            fun array ->
                let ft = array |> Finger.ofArray
                ft |> Finger.toArray = array

        testProperty "Tree of list must go back to list" <|
            fun list ->
                let ft = list |> Finger.ofList
                ft |> Finger.toList = list

        testProperty "Prepended items must show up at the front" <|
            fun (list:PosInt list) (NegInt elm) ->
                (List.length list > 0) ==> lazy (
                        // Remove enclosing type.
                        let list = List.map int list

                        let ft = list |> Finger.ofList
                        let added = ft |> Finger.prepend elm

                        // Make sure, no negatives are in there...
                        list |> List.forall (flip (>=) 0)
                        // ... and that our candidate is negative.
                     && elm < 0
                     && List.length list > 0
                     && Finger.length ft > 0
                     && ft |> Finger.head <> elm
                     && added |> Finger.head = elm
                    )

        testProperty "Appended items must show up at the end" <|
            fun (list:PosInt list) (NegInt elm) ->
                (List.length list > 0) ==> lazy (
                        // Remove enclosing type.
                        let list = List.map int list

                        let ft = list |> Finger.ofList
                        let added = ft |> Finger.append elm

                        // Make sure, no negatives are in there...
                        list |> List.forall (flip (>=) 0)
                        // ... and that our candidate is negative.
                     && elm < 0
                     && List.length list > 0
                     && Finger.length ft > 0
                     && ft |> Finger.last <> elm
                     && added |> Finger.last = elm
                    )

        testProperty "Concatenating adds two lengths" <|
            fun list1 list2 ->
                let c1 = List.length list1
                let c2 = List.length list2
                let tree1 = list1 |> Finger.ofList<string>
                let tree2 = list2 |> Finger.ofList
                let concat = Finger.concat tree1 tree2
                Finger.length concat = c1 + c2

        testProperty "Concatenation is the same as n appends" <|
            fun left right ->
                let tree = left |> Finger.ofList<char>
                let concat = right |> Finger.ofList |> Finger.concat tree
                let alterative = right |> List.fold (flip Finger.append) tree
                Finger.toList concat = Finger.toList alterative

        testProperty "Reversed concatenation is the same as n prepends" <|
            fun left right ->
                let tree = right |> Finger.ofList<decimal>
                let concat = Finger.concat (left |> Finger.ofList) tree
                let alterative = tree |> List.foldBack Finger.prepend left
                Finger.toList concat = Finger.toList alterative
    ]
