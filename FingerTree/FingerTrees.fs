namespace CmdQ.FingerTree

open Error
open Monoids
open CmdQ.FingerTree.Monoidal

module RandomAccess =
    open RandomAccess

    let inline ofSomething f1 f2 = (f1 Value) >> f2 (flip FingerTree.append) FingerTree.empty

    let ofList<'a> = List.map Value >> FingerTree.ofList

    let ofSeq<'a> = Seq.map Value >> FingerTree.ofSeq

    let ofArray<'a> = Array.map Value >> FingerTree.ofArray

    let inline unpack (Value v) = v

    let toSeq<'a> = FingerTree.toSeq >> Seq.map unpack

    let toList<'a> = FingerTree.toList >> List.map unpack

    let toArray<'a> = FingerTree.toArray >> Array.map unpack

    let tryItem index tree =
        let total:Size = fmeasure tree
        if index < 0 || index >= total.Value then None else
        let (Split(_, elm, _)) = tree |> FingerTree.split (fun x -> x.Value > index) (Size())
        Some elm

    let item index tree =
        match tryItem index tree with
        | Some elm -> elm
        | _ -> invalidIndex Messages.indexOutOfRange
