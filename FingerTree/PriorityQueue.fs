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

let add item priority : Tree<_> -> Tree<_> = FingerTree.append { Priority = priority; Value = item }

let inline ofSomething f sth : Tree<_> = sth |> f (fun acc (i, p) -> acc |> add i p) Empty

/// Create a finger tree from a sequence.
let ofList list = list |> ofSomething List.fold

/// Create a finger tree from a list.
let ofSeq seq = seq |> ofSomething Seq.fold

/// Create a finger tree from an array.
let ofArray array = array |> ofSomething Array.fold
