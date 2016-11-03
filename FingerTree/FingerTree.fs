namespace CmdQ.FingerTree

open Error

/// A 2-3-Node can hold either 2 or 3 elements.
type Node<'a> =
    | Node2 of 'a * 'a
    | Node3 of 'a * 'a * 'a

module Node =
    /// Convert a 2-3-Node to a list.
    let toList = function
        | Node2(a, b) -> [a; b]
        | Node3(a, b, c) -> [a; b; c]

    /// Convert a list of nodes to a list of deeper nodes so that the length shrinks to about 50 %.
    let rec toNodeList = function
        | []
        | [_] -> invalidOp "No enough elements."
        | [x; y] -> [Node2(x, y)]
        | [x; y; z] -> [Node3(x, y, z)]
        | x::y::rest -> Node2(x, y)::(toNodeList rest)

/// A digit holds at least 1 and up to 4 elements.
type Digit<'a> =
    | One of 'a
    | Two of 'a * 'a
    | Three of 'a * 'a * 'a
    | Four of 'a * 'a * 'a * 'a

/// A finger tree is either empty, holds a single elements or gets recursive with a prefix and a suffix of digits.
[<NoComparison>]
[<NoEquality>]
type FingerTree<'a> =
    | Empty
    | Single of 'a
    | Deep of Digit<'a> * Lazy<FingerTree<Node<'a>>> * Digit<'a>

/// A digit holds at least 1 and up to 4 elements.
module Digit =
    /// Maximum number of elements per digit.
    let max = 4

    /// Convert a list of values to a digit.
    let ofList = function
        | [a] -> One(a)
        | [a; b] -> Two(a, b)
        | [a; b; c] -> Three(a, b, c)
        | [a; b; c; d] -> Four(a, b, c, d)
        | _ -> failwith Messages.onlyList1to4Accepted

    /// Convert a digit to a list.
    let toList = function
        | One a -> [a]
        | Two(a, b) -> [a; b]
        | Three(a, b, c) -> [a; b; c]
        | Four(a, b, c, d) -> [a; b; c; d]

    /// Append a value to the right side of a digit.
    let append x = function
        | One a -> Two(a, x)
        | Two(a, b) -> Three(a, b, x)
        | Three(a, b, c) -> Four(a, b, c, x)
        | _ -> failwith Messages.digitAlreadyHas4Entries

    /// Prepend a value to the left side of a digit.
    let prepend x = function
        | One a -> Two(x, a)
        | Two(a, b) -> Three(x, a, b)
        | Three(a, b, c) -> Four(x, a, b, c)
        | _ -> failwith Messages.digitAlreadyHas4Entries

    /// Promote a digit to a finger tree.
    let promote = function
        | One a -> Single a
        | Two(a, b) -> Deep(One a, Lazy.CreateFromValue Empty, One b)
        | Three(a, b, c) -> Deep(Two(a, b), Lazy.CreateFromValue Empty, One(c))
        | Four(a, b, c, d) -> Deep(Two(a, b), Lazy.CreateFromValue Empty, Two(c, d))

    /// Active pattern to get the left-most element and the rest to the right.
    let (|SplitFirst|_|) = function
        | Two(a, b) -> Some(a, One b)
        | Three(a, b, c) -> Some(a, Two(b, c))
        | Four(a, b, c, d) -> Some(a, Three(b, c, d))
        | _ -> None

    /// Active pattern to get the right-most element and the rest to the left.
    let (|SplitLast|_|) = function
        | Two(a, b) -> Some(One a, b)
        | Three(a, b, c) -> Some(Two(a, b), c)
        | Four(a, b, c, d) -> Some(Three(a, b, c), d)
        | _ -> None

/// A View is either empty or points to an element in the finger tree and its position.
type View<'a> = Nil | View of 'a * Lazy<FingerTree<'a>>

module ConcatDeque =
    /// Return both the left-most element and the remaining tree (lazily).
    let rec viewl<'a> : FingerTree<'a> -> View<'a> = function
        | Empty -> Nil
        | Single x -> View(x, Lazy.CreateFromValue Empty)
        | Deep(One x, deeper, suffix) ->
            let rest = lazy (
                match viewl deeper.Value with
                | Nil ->
                    suffix |> Digit.promote
                | View (node, lazyRest) ->
                    let prefix = node |> Node.toList |> Digit.ofList
                    Deep(prefix, lazyRest, suffix)
            )
            View(x, rest)
        | Deep(Digit.SplitFirst(x, shorter), deeper, suffix) ->
            View(x, lazy Deep(shorter, deeper, suffix))
        | _ -> failwith Messages.patternMatchImpossible

    /// Return both the right-most element and the remaining tree (lazily).
    let rec viewr<'a> : FingerTree<'a> -> View<'a> = function
        | Empty -> Nil
        | Single x -> View(x, Lazy.CreateFromValue Empty)
        | Deep(prefix, deeper, One x) ->
            let rest = lazy (
                match viewr deeper.Value with
                | Nil ->
                    prefix |> Digit.promote
                | View (node, lazyRest) ->
                    let suffix = node |> Node.toList |> Digit.ofList
                    Deep(prefix, lazyRest, suffix)
            )
            View(x, rest)
        | Deep(prefix, deeper, Digit.SplitLast(shorter, x)) ->
            View(x, lazy Deep(prefix, deeper, shorter))
        | _ -> failwith Messages.patternMatchImpossible

    /// The empty finger tree.
    let empty = Empty

    /// Tests whether a finger tree is empty.
    let isEmpty tree =
        match viewl tree with
        | Nil -> true
        | _ -> false

    /// Return the head of the finger tree (i.e. the left-most element).
    let head tree =
        match viewl tree with
        | View(h, _) -> h
        | _ -> invalidArg "tree" Messages.treeIsEmpty

    /// Return the tail of the tree, i.e. all but the first element.
    let tail tree =
        match viewl tree with
        | View(_, Lazy t) -> t
        | _ -> invalidArg "tree" Messages.treeIsEmpty

    /// Return the last (i.e. the right-most) element of the finger tree.
    let last tree =
        match viewr tree with
        | View(h, _) -> h
        | _ -> invalidArg "tree" Messages.treeIsEmpty

    /// Return the spine of the tree, i.e. all but the last element.
    let butLast tree =
        match viewr tree with
        | View(_, Lazy spine) -> spine
        | _ -> invalidArg "tree" Messages.treeIsEmpty

    /// Return both the head and the tail at once.
    let (|PopLeft|_|) tree =
        match viewl tree with
        | View(head, Lazy tail) ->
            Some(head, tail)
        | _ -> None

    /// Return both the spine and the last element at once.
    let (|PopRight|_|) tree =
        match viewr tree with
        | View(last, Lazy butLast) ->
            Some(butLast, last)
        | _ -> None

    /// Append an element to the right of a tree.
    let rec append<'a> (z:'a) : FingerTree<'a> -> FingerTree<'a> = function
        | Empty -> Single z
        | Single y -> Deep(One y, Lazy.CreateFromValue Empty, One z)
        | Deep(prefix, Lazy deeper, Four(v, w, x, y)) ->
            // Force evaluation here, because the dept has already been paid for.
            Deep(prefix, Lazy.CreateFromValue(append (Node3(v, w, x)) deeper), Two(y, z))
        | Deep(prefix, deeper, suffix) ->
            Deep(prefix, deeper, suffix |> Digit.append z)

    /// Prepend an element to the left of a tree.
    let rec prepend<'a> (a:'a) : FingerTree<'a> -> FingerTree<'a> = function
        | Empty -> Single a
        | Single b -> Deep(One a, Lazy.CreateFromValue Empty, One b)
        | Deep(Four(b, c, d, e), Lazy deeper, suffix) ->
            // Force evaluation here, because the dept has already been paid for.
            Deep(Two(a, b), Lazy.CreateFromValue(prepend (Node3(c, d, e)) deeper), suffix)
        | Deep(prefix, deeper, suffix) ->
            Deep(prefix |> Digit.prepend a, deeper, suffix)

    let inline ofSomething f = f (flip append) empty

    /// Create a finger tree from a sequence.
    let ofSeq sth = ofSomething Seq.fold sth

    /// Create a finger tree from a list.
    let ofList sth = ofSomething List.fold sth

    /// Create a finger tree from an array.
    let ofArray arr = Array.foldBack prepend arr empty

    /// Convert a tree to a sequence, i.e. enumerate all elements left to right.
    let rec toSeq<'a> (tree:FingerTree<'a>) : seq<'a> = seq {
        match tree with
        | Single single ->
            yield single
        | Deep(prefix, Lazy deeper, suffix) ->
            yield! prefix |> Digit.toList
            yield! deeper |> toSeq |> Seq.collect Node.toList
            yield! suffix |> Digit.toList
        | Empty -> ()
    }

    /// Convert a tree to an array (left to right).
    let toArray<'a> = toSeq<'a> >> Seq.toArray

    /// Convert a tree to a list (left to right).
    let rec toList tree =
        match viewl tree with
        | Nil -> []
        | View(head, Lazy tail) -> head::(toList tail)

    /// Concatenate two trees while putting a list of elements in the middle.
    let rec concatWithMiddle<'a> : FingerTree<'a> * 'a list * FingerTree<'a> -> FingerTree<'a> = function
        | Empty, [], only
        | only, [], Empty -> only
        | Empty, left::rest, right
        | Single left, rest, right -> concatWithMiddle(Empty, rest, right) |> prepend left
        | left, List.Snoc(rest, right), Empty
        | left, rest, Single right -> concatWithMiddle(left, rest, Empty) |> append right
        | Deep(leftPrefix, leftDeeper, leftSuffix), middle, Deep(rightPrefix, rightDeeper, rightSuffix) ->
            let deeper = lazy (
                let leftList = leftSuffix |> Digit.toList
                let rightList = rightPrefix |> Digit.toList
                let middle' = leftList @ middle @ rightList |> Node.toNodeList
                concatWithMiddle(leftDeeper.Value, middle', rightDeeper.Value)
            )
            Deep(leftPrefix, deeper, rightSuffix)
        | _ -> failwith Messages.patternMatchImpossible

    /// Concatenate two finger trees.
    let concat left right = concatWithMiddle(left, [], right)

    /// Apply a mapping to a sequence and merge all resulting finger trees into one.
    let collect mapping = Seq.map mapping >> Seq.fold concat empty
