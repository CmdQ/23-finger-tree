namespace CmdQ.FingerTree

open Error
open Monoids
open Monoids.RandomAccess
open CmdQ.FingerTree.Monoidal

/// Functions for querying and manipulating random access finger trees.
module RandomAccess =
    /// A shorthand type for a finger tree.
    type Tree<'a> = FingerTree<Size, Value<'a>>

    let inline unpack (Value v) = v

    /// The empty tree.
    let empty : Tree<_> = Empty

    /// Checks whether a tree is empty.
    let isEmpty : Tree<_> -> bool = function
        | Empty -> true
        | _ -> false

    /// Append an element to the right of a tree.
    let append value (tree:Tree<_>) = tree |> FingerTree.append (Value value)

    /// Prepend an element to the left of a tree.
    let prepend value (tree:Tree<_>) = tree |> FingerTree.prepend (Value value)

    let inline ofSomething f sth : Tree<_> = sth |> f (flip append) Empty

    /// Create a finger tree from a sequence.
    let ofList list = list |> ofSomething List.fold

    /// Create a finger tree from a list.
    let ofSeq seq = seq |> ofSomething Seq.fold

    /// Create a finger tree from an array.
    let ofArray array = array |> ofSomething Array.fold

    /// Convert a tree to a list (left to right).
    let toList tree =
        let rec toList acc (tree:Tree<_>) =
            match FingerTree.viewr tree with
            | Nil -> acc
            | View(Value head, Lazy tail) -> toList (head::acc) tail
        toList [] tree

    /// Convert a tree to a sequence, i.e. enumerate all elements left to right.
    let toSeq tree = tree |> (toList >> List.toSeq)

    /// Return the number of elements in the tree.
    let length (tree:Tree<_>) = (fmeasure tree).Value

    /// Convert a tree to an array (left to right).
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

    /// Concatenate two finger trees.
    let concat (left:Tree<_>) (right:Tree<_>) : Tree<_> = FingerTree.concat left right

    /// Return the head of the finger tree (i.e. the left-most element).
    let head (tree:Tree<_>) =
        match FingerTree.viewl tree with
        | View(Value v, _) -> v
        | _ -> invalidArg "tree" Messages.treeIsEmpty

    /// Return the tail of the tree, i.e. all but the first element.
    let tail (tree:Tree<_>) : Tree<_> =
        match FingerTree.viewl tree with
        | View(_, Lazy t) -> t
        | _ -> invalidArg "tree" Messages.treeIsEmpty

    /// Return the last (i.e. the right-most) element of the finger tree.
    let last (tree:Tree<_>) =
        match FingerTree.viewr tree with
        | View(Value v, _) -> v
        | _ -> invalidArg "tree" Messages.treeIsEmpty

    /// Return the spine of the tree, i.e. all but the last element.
    let butLast (tree:Tree<_>) : Tree<_> =
        match FingerTree.viewr tree with
        | View(_, Lazy t) -> t
        | _ -> invalidArg "tree" Messages.treeIsEmpty

    /// Apply a mapping to a sequence and merge all resulting finger trees into one.
    let collect mapping = Seq.map mapping >> Seq.fold concat Empty

    let inline outsideError () = invalidIndex Messages.indexOutOfRange

    let indexChecked whenOutside whenOk index (tree:Tree<_>) =
        let total = fmeasure tree
        if index < 0 || index >= total.Value then
            whenOutside()
        else
            whenOk(index, tree)

    /// Return the element at a given position.
    let item index tree =
        indexChecked outsideError (fun (index, tree) ->
            let (Split(_, Value elm, _)) = tree |> FingerTree.split (fun x -> x.Value > index) (Size())
            elm
        ) index tree

    /// Return the element at a given position.
    let get tree index = item index tree

    /// Return a new tree where a given position is replaced with a new value.
    let set tree index value : Tree<_> =
        indexChecked outsideError (fun (index, tree) ->
            let (Split(left, _, right)) = tree |> FingerTree.split (fun x -> x.Value > index) (Size())
            FingerTree.prepend (Value value) right |> FingerTree.concat left
        ) index tree

    /// Remove an element at a given position from the tree.
    let removeIndex index tree : Tree<_> =
        indexChecked outsideError (fun (index, tree) ->
            let (Split(left, _, right)) = tree |> FingerTree.split (fun x -> x.Value > index) (Size())
            FingerTree.concat left right
        ) index tree

    /// Return the depth of a tree while also checking that all measures are correct.
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