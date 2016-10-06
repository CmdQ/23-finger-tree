module CmdQ.Tests.FingerTreeTest

open MyArbitraries
open CmdQ
open Fuchu
open FsCheck
open Swensen.Unquote

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
                Finger.length ft =! 0
                (ft |> Finger.append elm |> Finger.length) =! 1
                (ft |> Finger.prepend elm |> Finger.length) =! 1

        testProperty "Appending increases size" <|
            fun coll elm1 elm2 ->
                let ft = coll |> Finger.ofArray
                let before = Finger.length ft
                let bigger = ft |> Finger.append elm1
                let after = Finger.length bigger
                after =! before + 1
                let evenBigger = bigger |> Finger.prepend elm2
                Finger.length evenBigger =! after + 1

        testProperty "Tree of array must go back to array" <|
            fun array ->
                let ft = array |> Finger.ofArray
                ft |> Finger.toArray =! array

        testProperty "Tree of list must go back to list" <|
            fun list ->
                let ft = list |> Finger.ofList
                ft |> Finger.toList =! list

        testProperty "Prepended items must show up at the front" <|
            fun (list:PosInt list) (NegInt elm) ->
                (List.length list > 0) ==> lazy (
                        // Remove enclosing type.
                        let list = List.map int list
                        // Make sure, no negatives are in there...
                        list |> List.forall (flip (>=) 0) =! true
                        // ... and that our candidate is negative.
                        elm <! 0

                        List.length list >! 0
                        let ft = list |> Finger.ofList
                        Finger.length ft >! 0
                        let added = ft |> Finger.prepend elm
                        ft |> Finger.head <>! elm
                        added |> Finger.head =! elm
                    )

        testProperty "Appended items must show up at the end" <|
            fun (list:PosInt list) (NegInt elm) ->
                (List.length list > 0) ==> lazy (
                        // Remove enclosing type.
                        let list = List.map int list
                        // Make sure, no negatives are in there...
                        list |> List.forall (flip (>=) 0) =! true
                        // ... and that our candidate is negative.
                        elm <! 0

                        List.length list >! 0
                        let ft = list |> Finger.ofList
                        Finger.length ft >! 0
                        let added = ft |> Finger.append elm
                        ft |> Finger.last <>! elm
                        added |> Finger.last =! elm
                    )
    ]