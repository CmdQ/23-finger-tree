module CmdQ.FingerTree.PriorityQueue

open CmdQ.FingerTree.Monoidal
open Monoids
open Monoids.PriorityQueue

/// A shorthand type for a finger tree.
type Tree<'a> = FingerTree<Priority, PriorityAndValue<'a>>

let empty : Tree<_> = Empty

/// Checks whether a tree is empty.
let isEmpty : Tree<_> -> bool = function
    | Empty -> true
    | _ -> false

let add item priority : Tree<_> -> Tree<_> = FingerTree.prepend { Priority = priority; Value = item }

let dowith (tree:Tree<_>) =
    let maxp = (fmeasure tree).Priority
    tree |> FingerTree.split (fun x -> x.Priority >= maxp) (Priority())

let peek tree =
    let (Split(_, { Value = v }, _)) = tree |> dowith
    v

let pop tree =
    let (Split(l, _, r)) = tree |> dowith
    FingerTree.concat l r

let (|Pop|_|) tree =
    if isEmpty tree then None else
    let (Split(l, {Value = m}, r)) = tree |> dowith
    Some(m, ((FingerTree.concat l r):Tree<_>))

let inline ofSomething f sth : Tree<_> = sth |> f (fun acc (i, p) -> acc |> add i p) Empty

/// Create a finger tree from a sequence.
let ofList list = list |> ofSomething List.fold

/// Create a finger tree from a list.
let ofSeq seq = seq |> ofSomething Seq.fold

/// Create a finger tree from an array.
let ofArray array = array |> ofSomething Array.fold
