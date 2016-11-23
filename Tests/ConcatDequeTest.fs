module CmdQ.FingerTree.Tests.ConcatDequeTest

open CmdQ.FingerTree
open MyArbitraries
open Fuchu
open FsCheck
open Swensen.Unquote
open System.Diagnostics

module ConcatDeque =
    let length<'a> = ConcatDeque.toSeq<'a> >> Seq.length

    let rec depth<'a> (d:int) : FingerTree<'a> -> int = function
        | Empty -> d
        | Single _ -> d + 1
        | Deep(_, Lazy deeper, _) ->
            depth (d + 1) deeper

type OnlyInRelease(length) as this =

    let mutable isDebug = false

    do this.SetFlag()

    member private me.SetFlag () =
        me.OnlyInDebug()

    [<Conditional("DEBUG")>]
    member private __.OnlyInDebug () =
        isDebug <- true

    member __.OnlyInRelease() =
        if not isDebug then
            let tree = seq { 1..length } |> ConcatDeque.ofSeq
            tree |> ConcatDeque.toList
        else
            [42]

[<Tests>]
let stackOverflowTest =
    let length = 999999
    let mode = OnlyInRelease(length)
    let list = mode.OnlyInRelease()
    List.isEmpty list =! false

[<Tests>]
let properties =
    testList "ConcatDeque" [
        testProperty "Empty finger tree, one insertion" <|
            fun elm ->
                let ft = ConcatDeque.empty
                ConcatDeque.length ft = 0
             && (ft |> ConcatDeque.append elm |> ConcatDeque.length) = 1
             && (ft |> ConcatDeque.prepend elm |> ConcatDeque.length) = 1

        testProperty "Appending increases size" <|
            fun coll elm1 elm2 ->
                let ft = coll |> ConcatDeque.ofArray
                let before = ConcatDeque.length ft
                let bigger = ft |> ConcatDeque.append elm1
                let after = ConcatDeque.length bigger
                let evenBigger = bigger |> ConcatDeque.prepend elm2
                after = before + 1 && ConcatDeque.length evenBigger = after + 1

        testProperty "Tree of array must go back to array" <|
            fun array ->
                let ft = array |> ConcatDeque.ofArray
                ft |> ConcatDeque.toArray = array

        testProperty "Tree of list must go back to list" <|
            fun list ->
                let ft = list |> ConcatDeque.ofList
                ft |> ConcatDeque.toList = list

        testProperty "Tree of list must go back to seq" <|
            fun list ->
                let ft = list |> List.toSeq |> ConcatDeque.ofSeq
                ft |> ConcatDeque.toSeq |> Seq.toList = list

        testProperty "Prepended items must show up at the front" <|
            fun (list:PosInt list) (NegInt elm) ->
                (not (List.isEmpty list)) ==> lazy (
                        // Remove enclosing type.
                        let list = List.map int list

                        let ft = list |> ConcatDeque.ofList
                        let added = ft |> ConcatDeque.prepend elm

                        // Make sure, no negatives are in there...
                        list |> List.forall (flip (>=) 0)
                        // ... and that our candidate is negative.
                     && elm < 0
                     && not (List.isEmpty list)
                     && ConcatDeque.length ft > 0
                     && ft |> ConcatDeque.head <> elm
                     && added |> ConcatDeque.head = elm
                    )

        testProperty "Appended items must show up at the end" <|
            fun (list:PosInt list) (NegInt elm) ->
                (not (List.isEmpty list)) ==> lazy (
                        // Remove enclosing type.
                        let list = List.map int list

                        let ft = list |> ConcatDeque.ofList
                        let added = ft |> ConcatDeque.append elm

                        // Make sure, no negatives are in there...
                        list |> List.forall (flip (>=) 0)
                        // ... and that our candidate is negative.
                     && elm < 0
                     && not (List.isEmpty list)
                     && ConcatDeque.length ft > 0
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
                ConcatDeque.length concat = c1 + c2

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
                ConcatDeque.toArray concat = ConcatDeque.toArray alterative

        testProperty "PopLeft reduces size of tree by one iff it has elements" <|
            fun list ->
                let tree = list |> ConcatDeque.ofArray
                match tree with
                | ConcatDeque.PopLeft(_, smaller) ->
                    ConcatDeque.length tree = ConcatDeque.length smaller + 1
                | _ ->
                    ConcatDeque.length tree = 0

        testProperty "PopRight reduces size of tree by one iff it has elements" <|
            fun list ->
                let tree = list |> ConcatDeque.ofList
                match tree with
                | ConcatDeque.PopRight(smaller, _) ->
                    ConcatDeque.length tree = ConcatDeque.length smaller + 1
                | _ ->
                    ConcatDeque.length tree = 0

        testProperty "Head of tree is same as first element in array" <|
            fun elms ->
                let tree:FingerTree<uint64> = elms |> ConcatDeque.ofArray
                elms.Length = 0 || ConcatDeque.head tree = elms.[0]

        testProperty "Last element of tree is same as last in array" <|
            fun elms ->
                let tree:FingerTree<int64> = elms |> ConcatDeque.ofArray
                elms.Length = 0 || ConcatDeque.last tree = elms.[elms.Length - 1]

        testProperty "Tail of list is same as tail of tree from list" <|
            fun elms ->
                let tree:FingerTree<int16> = elms |> ConcatDeque.ofList
                List.isEmpty elms || tree |> ConcatDeque.tail |> ConcatDeque.toList = (elms |> List.tail)

        testProperty "Spine of array is same as spine of tree from array" <|
            fun elms ->
                let tree:FingerTree<uint16> = elms |> ConcatDeque.ofArray
                elms.Length <= 1 || tree |> ConcatDeque.butLast |> ConcatDeque.toArray = elms.[..elms.Length - 2]

        testProperty "Reversing twice undoes." <|
            fun elms ->
                let tree:FingerTree<int> = elms |> ConcatDeque.ofArray
                (tree |> ConcatDeque.rev |> ConcatDeque.rev |> ConcatDeque.toArray) = elms

        testProperty "Reversing is the same as reversing input." <|
            fun elms ->
                let tree:FingerTree<int> = elms |> ConcatDeque.ofArray
                let lhs = tree |> ConcatDeque.rev |> ConcatDeque.toArray
                let rhs = Array.rev elms
                lhs = rhs
    ]
