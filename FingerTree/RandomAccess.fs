namespace CmdQ.FingerTree

open Error
open Monoids
open Monoids.RandomAccess
open CmdQ.FingerTree.Monoidal

module RandomAccess =
    type Tree<'a> = FingerTree<Size, Value<'a>>

    let inline unpack (Value v) = v

    let empty : Tree<_> = Empty

    let isEmpty : Tree<_> -> bool = function
        | Empty -> true
        | _ -> false

    let prepend value (tree:Tree<_>) = tree |> FingerTree.prepend (Value value)

    let append value (tree:Tree<_>) = tree |> FingerTree.append (Value value)

    let inline ofSomething f sth : Tree<_> = sth |> f (flip append) Empty

    let ofList list = list |> ofSomething List.fold

    let ofSeq seq = seq |> ofSomething Seq.fold

    let ofArray array = array |> ofSomething Array.fold

    let toList tree =
        let rec toList acc (tree:Tree<_>) =
            match FingerTree.viewr tree with
            | View(Value v, Lazy rest) ->
                rest |> toList (v::acc)
            | Nil -> acc
        toList [] tree

    let toSeq tree = tree |> (toList >> List.toSeq)

    let length (tree:Tree<_>) = (fmeasure tree).Value

    let toArray (tree:Tree<_>) =
        let len = length tree
        let re = Array.zeroCreate len
        let rec toArray i tree =
            match FingerTree.viewl tree with
            | View(Value v, Lazy rest) ->
                re.[i] <- v
                toArray (i + 1) rest
            | Nil ->
                assert(i = re.Length)
        toArray 0 tree
        re

    let concat (left:Tree<_>) (right:Tree<_>) : Tree<_> = FingerTree.concat left right

    let head (tree:Tree<_>) =
        match FingerTree.viewl tree with
        | View(Value v, _) -> v
        | _ -> invalidArg "tree" Messages.treeIsEmpty

    let tail (tree:Tree<_>) : Tree<_> =
        match FingerTree.viewl tree with
        | View(_, Lazy t) -> t
        | _ -> invalidArg "tree" Messages.treeIsEmpty

    let last (tree:Tree<_>) =
        match FingerTree.viewr tree with
        | View(Value v, _) -> v
        | _ -> invalidArg "tree" Messages.treeIsEmpty

    let butLast (tree:Tree<_>) : Tree<_> =
        match FingerTree.viewr tree with
        | View(_, Lazy t) -> t
        | _ -> invalidArg "tree" Messages.treeIsEmpty

    let collect mapping = Seq.map mapping >> Seq.fold concat Empty

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

    let rec depthCheck<'a
        when 'a :> IMeasured<Size>
        > : FingerTree<Size, 'a> -> int = function
        | Empty -> 0
        | Single _ -> 1
        | Deep(total, prefix, Lazy deeper, suffix) ->
            let measured = (fmeasure prefix).Value + (fmeasure deeper).Value + (fmeasure suffix).Value
            if total.Value <> measured then
                System.ApplicationException(sprintf "The measures are not consistent. %i is cached but %i was measured." total.Value measured) |> raise
            else
                depthCheck deeper + 1