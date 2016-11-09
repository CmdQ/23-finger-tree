namespace CmdQ.FingerTree

open Error
open Monoids
open Monoids.RandomAccess
open CmdQ.FingerTree.Monoidal

module RandomAccess =
    type Tree<'a> = FingerTree<Size, Value<'a>>

    let inline unpack (Value v) = v

    let empty : Tree<_> = FingerTree.empty

    let isEmpty : Tree<_> -> bool = function
        | Empty -> true
        | _ -> false

    let prepend value (tree:Tree<_>) = tree |> FingerTree.prepend (Value value)

    let append value (tree:Tree<_>) = tree |> FingerTree.append (Value value)

    let inline ofSomething f sth : Tree<_> = sth |> f (flip append) Empty

    let ofList list = list |> ofSomething List.fold

    let ofSeq seq = seq |> ofSomething Seq.fold

    let ofArray array = array |> ofSomething Array.fold

    let rec toList (tree:Tree<_>) =
        match FingerTree.viewl tree with
        | View(Value v, Lazy rest) ->
            v::toList rest
        | Nil -> []

    let toSeq tree = tree |> (toList >> List.toSeq)

    let length (tree:Tree<_>) = (fmeasure tree).Value

    let toArray (tree:Tree<_>) =
        let len = length tree
        let re = Array.zeroCreate len
        let rec toArray i tree =
            match FingerTree.viewl tree with
            | View(Value v, Lazy rest) ->
                re.[i] <- v
                rest |> toArray (i + 1)
            | Nil ->
                assert(i = re.Length)
        toArray 0 tree
        re

    let outsideError () = invalidIndex Messages.indexOutOfRange

    let indexChecked whenOutside whenOk index (tree:Tree<_>) =
        let total = fmeasure tree
        if index < 0 || index >= total.Value then
            whenOutside()
        else
            whenOk(index, tree)

    let tryItem index tree =
        indexChecked (fun () -> None) (fun (index, tree) ->
            let (Split(_, Value elm, _)) = tree |> FingerTree.split (fun x -> x.Value > index) (Size())
            Some elm
        ) index tree

    let item index tree =
        match tryItem index tree with
        | Some elm -> elm
        | _ -> outsideError()

    let get tree index = item index tree

    let set tree index value : Tree<_> =
        indexChecked outsideError (fun (index, tree) ->
            let (Split(left, _, right)) = tree |> FingerTree.split (fun x -> x.Value > index) (Size())
            FingerTree.prepend (Value value) right |> FingerTree.concat left
        ) index tree

    let removeIndex tree index : Tree<_> =
        indexChecked outsideError (fun (index, tree) ->
            let (Split(left, _, right)) = tree |> FingerTree.split (fun x -> x.Value > index) (Size())
            FingerTree.concat left right
        ) index tree
