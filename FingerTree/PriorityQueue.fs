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

/// Add an item with a priority to the queue.
let add item priority : Tree<_> -> Tree<_> = FingerTree.prepend { Priority = priority; Value = item }

/// Return the union of two priority queues.
let union (x:Tree<_>) (y:Tree<_>) : Tree<_> = FingerTree.concat x y

let private singletonPriority = Priority()
let private split (tree:Tree<_>) =
    let maxp = (fmeasure tree).Priority
    tree |> FingerTree.split (fun x -> x.Priority >= maxp) singletonPriority

/// Peek at the item with the highest priority.
let peek tree =
    let (Split(_, { Value = v }, _)) = tree |> split
    v

/// Give back a queue where the item with the highest priority is removed.
let pop tree =
    let (Split(l, _, r)) = tree |> split
    union l r

/// Return both the item with the highest priority and the reduced queue.
let (|Pop|_|) tree =
    if isEmpty tree then None else
    let (Split(l, { Value = m }, r)) = tree |> split
    Some(m, ((union l r):Tree<_>))

let inline private ofSomething f sth : Tree<_> = sth |> f (fun acc (i, p) -> acc |> add i p) Empty

/// Create a finger tree from a sequence.
let ofList list = list |> ofSomething List.fold

/// Create a finger tree from a list.
let ofSeq seq = seq |> ofSomething Seq.fold

/// Create a finger tree from an array.
let ofArray array = array |> ofSomething Array.fold

/// Enumerate all values with their priorities.
let rec toSeqWithPriorities tree = seq {
    if not <| isEmpty tree then
        let (Split(l, m, r)) = tree |> split
        yield m
        yield! union l r |> toSeqWithPriorities
}

/// Enumerate all values in the queue from highest to lowest priority.
let toSeq tree = tree |> (toSeqWithPriorities >> Seq.map (fun { Value = v } -> v))

/// Convert queue to a list. The elements in the list will be ordered from highest to lowest priority.
let toList tree = tree |> (toSeq >> Seq.toList)

/// Convert queue to an array. The elements in the list will be ordered from highest to lowest priority.
let toArray tree = tree |> (toSeq >> Seq.toArray)
