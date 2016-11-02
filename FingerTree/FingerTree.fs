namespace CmdQ

open System
open Error
open Monoid

/// A 2-3-Node can hold either 2 or 3 elements.
type Node<'m, 'a when 'm :> IMonoid<'m>> =
    | Node2 of 'm * 'a * 'a
    | Node3 of 'm * 'a * 'a * 'a

    interface IMeasured<'m, Node<'m, 'a>> with
        member me.Measure =
            match me with
            | Node2(v, _, _) -> v
            | Node3(v, _, _, _) -> v

module Node =
    /// Convert a list to a 2-3-Node.
    let ofList list =
        match list with
        | [a; b] -> Node2(mconcat list, a, b)
        | [a; b; c] -> Node3(mconcat list, a, b, c)
        | _ -> failwith Messages.onlyList2or3Accepted

    /// Convert a 2-3-Node to a list.
    let toList = function
        | Node2(_, a, b) -> [a; b]
        | Node3(_, a, b, c) -> [a; b; c]

    /// Convert a list of nodes to a list of deeper nodes so that the length shrinks to about 50 %.
    let rec toNodeList list =
        match list with
        | []
        | [_] -> invalidOp "No enough elements."
        | [x; y] -> [Node2(mconcat list, x, y)]
        | [x; y; z] -> [Node3(mconcat list, x, y, z)]
        | x::y::rest -> Node2(mconcat list, x, y)::(toNodeList rest)

/// A digit holds at least 1 and up to 4 elements.
type Digit<'m, 'a
    when 'm :> IMonoid<'m>
        and 'a :> IMeasured<'m, 'a>
    > =
    | One of 'a
    | Two of 'a * 'a
    | Three of 'a * 'a * 'a
    | Four of 'a * 'a * 'a * 'a

    /// Convert a digit to a list.
    member me.ToList () =
        match me with
        | One a -> [a]
        | Two(a, b) -> [a; b]
        | Three(a, b, c) -> [a; b; c]
        | Four(a, b, c, d) -> [a; b; c; d]

    interface IMeasured<'m, 'a> with
        member me.Measure = me.ToList() |> mconcat

/// A finger tree is either empty, holds a single elements or gets recursive with a prefix and a suffix of digits.
[<NoComparison>]
[<NoEquality>]
type FingerTree<'m, 'a
    when 'm :> IMonoid<'m>
        and 'm : (new : unit -> 'm)
        and 'a :> IMeasured<'m, 'a>
    > =
    | Empty
    | Single of 'a
    | Deep of 'm * Digit<'m, 'a> * Lazy<FingerTree<'m, Node<'m, 'a>>> * Digit<'m, 'a>

    member __.Monoid = Singleton.Instance

    interface IMeasured<'m, 'a> with
        member me.Measure =
            match me with
            | Empty ->
                //fmeasure Unchecked.defaultof<'a>
                me.Monoid
            | Single x -> fmeasure x
            | Deep(v, _, _, _) -> v

module Digit =
    let max = 4

    /// Convert a list of values to a digit.
    let ofList = function
        | [a] -> One(a)
        | [a; b] -> Two(a, b)
        | [a; b; c] -> Three(a, b, c)
        | [a; b; c; d] -> Four(a, b, c, d)
        | _ -> invalidOp Messages.onlyList1to4Accepted

    /// Convert a digit to a list.
    let toList (digit:Digit<_, _>) = digit.ToList()

    /// Append a value to the right side of a digit.
    let append x = function
        | One a -> Two(a, x)
        | Two(a, b) -> Three(a, b, x)
        | Three(a, b, c) -> Four(a, b, c, x)
        | _ -> invalidOp Messages.digitAlreadyHas4Entries

    /// Prepend a value to the left side of a digit.
    let prepend x = function
        | One a -> Two(x, a)
        | Two(a, b) -> Three(x, a, b)
        | Three(a, b, c) -> Four(x, a, b, c)
        | _ -> invalidOp Messages.digitAlreadyHas4Entries

    /// Promote a digit to a finger tree.
    let promote = function
        | One a -> Single a
        | Two(a, b) -> Deep(mconcat [a; b], One a, Lazy.CreateFromValue Empty, One b)
        | Three(a, b, c) -> Deep(mconcat [a; b; c], Two(a, b), Lazy.CreateFromValue Empty, One(c))
        | Four(a, b, c, d) -> Deep(mconcat [a; b; c; d], Two(a, b), Lazy.CreateFromValue Empty, Two(c, d))

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
type View<'m, 'a
    when 'm :> IMonoid<'m>
        and 'm : (new : unit -> 'm)
        and 'a :> IMeasured<'m, 'a>
    > = Nil | View of 'a * Lazy<FingerTree<'m, 'a>>

/// A split points to an element in the finger tree and also provides the two finger trees around that element.
type Split<'m, 'a
    when 'm :> IMonoid<'m>
        and 'm : (new : unit -> 'm)
        and 'a :> IMeasured<'m, 'a>
    > = Split of FingerTree<'m, 'a> * 'a * FingerTree<'m, 'a>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FingerTree =
    /// Return both the left-most element and the remaining tree (lazily).
    let rec viewl<'m, 'a
        when 'm :> IMonoid<'m>
            and 'm : (new : unit -> 'm)
            and 'a :> IMeasured<'m, 'a>
        > : FingerTree<'m, 'a> -> View<'m, 'a> = function
        | Empty -> Nil
        | Single x -> View(x, Lazy.CreateFromValue Empty)
        | Deep(_, One x, deeper, suffix) as tree ->
            let rest = lazy (
                match viewl deeper.Value with
                | Nil ->
                    suffix |> Digit.promote
                | View (node, lazyRest) ->
                    let prefix = node |> Node.toList |> Digit.ofList
                    let v = (fmeasure prefix).Add(fmeasure lazyRest.Value).Add(fmeasure suffix)
                    Deep(v, prefix, lazyRest, suffix)
            )
            View(x, rest)
        | Deep(_, Digit.SplitFirst(x, shorter), deeper, suffix) ->
            let lazyRest = lazy (
                let v = (fmeasure shorter).Add(fmeasure deeper.Value).Add(fmeasure suffix)
                Deep(v, shorter, deeper, suffix)
            )
            View(x, lazyRest)

    /// Return both the right-most element and the remaining tree (lazily).
    let rec viewr<'m, 'a
        when 'm :> IMonoid<'m>
            and 'm : (new : unit -> 'm)
            and 'a :> IMeasured<'m, 'a>
        > : FingerTree<'m, 'a> -> View<'m, 'a> = function
        | Empty -> Nil
        | Single x -> View(x, Lazy.CreateFromValue Empty)
        | Deep(_, prefix, deeper, One x) ->
            let rest = lazy (
                match viewr deeper.Value with
                | Nil ->
                    prefix |> Digit.promote
                | View (node, lazyRest) ->
                    let suffix = node |> Node.toList |> Digit.ofList
                    let v = (fmeasure prefix).Add(fmeasure lazyRest.Value).Add(fmeasure suffix)
                    Deep(v, prefix, lazyRest, suffix)
            )
            View(x, rest)
        | Deep(_, prefix, deeper, Digit.SplitLast(shorter, x)) ->
            let lazyRest = lazy (
                let v = (fmeasure prefix).Add(fmeasure deeper.Value).Add(fmeasure shorter)
                Deep(v, prefix, deeper, shorter)
            )
            View(x, lazyRest)

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
    let rec append<'m, 'a
        when 'm :> IMonoid<'m>
            and 'm : (new : unit -> 'm)
            and 'a :> IMeasured<'m, 'a>
        > (z:'a) : FingerTree<'m, 'a> -> FingerTree<'m, 'a> = function
        | Empty -> Single z
        | Single y ->
            let annot = mconcat [y; z]
            Deep(annot, One y, Lazy.CreateFromValue Empty, One z)
        | Deep(annot, prefix, Lazy deeper, Four(v, w, x, y)) ->
            // Force evaluation here, because the dept has already been paid for.
            Deep(annot.Add (fmeasure z),
                prefix,
                Lazy.CreateFromValue(append (Node.ofList [v; w; x]) deeper),
                Two(y, z))
        | Deep(v, prefix, deeper, suffix) ->
            Deep(v.Add (fmeasure z), prefix, deeper, suffix |> Digit.append z)

    /// Prepend an element to the left of a tree.
    let rec prepend<'m, 'a
        when 'm :> IMonoid<'m>
            and 'm : (new : unit -> 'm)
            and 'a :> IMeasured<'m, 'a>
        > (a:'a) : FingerTree<'m, 'a> -> FingerTree<'m, 'a> = function
        | Empty -> Single a
        | Single b ->
            let annot = mconcat [a; b]
            Deep(annot, One a, Lazy.CreateFromValue Empty, One b)
        | Deep(annot, Four(b, c, d, e), Lazy deeper, suffix) ->
            // Force evaluation here, because the dept has already been paid for.
            Deep((fmeasure a).Add annot,
                Two(a, b),
                Lazy.CreateFromValue(prepend (Node.ofList [c; d; e]) deeper),
                suffix)
        | Deep(annot, prefix, deeper, suffix) ->
            Deep((fmeasure a).Add annot, prefix |> Digit.prepend a, deeper, suffix)

    let inline ofSomething modul = modul (flip append<'m, 'a>) empty

    /// Create a finger tree from a sequence.
    let ofSeq sth = ofSomething Seq.fold sth

    /// Create a finger tree from a list.
    let ofList sth = ofSomething List.fold sth

    /// Create a finger tree from an array.
    let ofArray arr = Array.foldBack prepend arr empty

    /// Convert a tree to a sequence, i.e. enumerate all elements left to right.
    let rec toSeq<'m, 'a
        when 'm :> IMonoid<'m>
            and 'm : (new : unit -> 'm)
            and 'a :> IMeasured<'m, 'a>
        > (tree:FingerTree<'m, 'a>) : seq<'a> = seq {
        match tree with
        | Single single ->
            yield single
        | Deep(_, prefix, Lazy deeper, suffix) ->
            yield! prefix |> Digit.toList
            yield! deeper |> toSeq |> Seq.collect Node.toList
            yield! suffix |> Digit.toList
        | Empty -> ()
    }

    /// Convert a tree to an array (left to right).
    let toArray<'m, 'a
        when 'm :> IMonoid<'m>
            and 'm : (new : unit -> 'm)
            and 'a :> IMeasured<'m, 'a>
        > = toSeq<'m, 'a> >> Seq.toArray

    /// Convert a tree to a list (left to right).
    let rec toList tree =
        match viewl tree with
        | Nil -> []
        | View(head, Lazy tail) -> head::(toList tail)

    /// Concatenate two trees while putting a list of elements in the middle.
    let rec
        concatWithMiddle<'m, 'a
            when 'm :> IMonoid<'m>
                and 'm : (new : unit -> 'm)
                and 'a :> IMeasured<'m, 'a>
        > : FingerTree<'m, 'a> * 'a list * FingerTree<'m, 'a> -> FingerTree<'m, 'a> = function
        | Empty, [], only
        | only, [], Empty -> only
        | Empty, left::rest, right
        | Single left, rest, right -> concatWithMiddle(Empty, rest, right) |> prepend left
        | left, List.Snoc(rest, right), Empty
        | left, rest, Single right -> concatWithMiddle(left, rest, Empty) |> append right
        | Deep(leftAnnot, leftPrefix, leftDeeper, leftSuffix), middle, Deep(rightAnnot, rightPrefix, rightDeeper, rightSuffix) ->
            let deeper = lazy (
                let leftList = leftSuffix |> Digit.toList
                let rightList = rightPrefix |> Digit.toList
                let middle' = leftList @ middle @ rightList |> Node.toNodeList
                concatWithMiddle(leftDeeper.Value, middle', rightDeeper.Value)
            )
            Deep(leftAnnot.Add rightAnnot, leftPrefix, deeper, rightSuffix)

    /// Concatenate two finger trees.
    let concat left right = concatWithMiddle(left, [], right)

    /// Apply a mapping to a sequence and merge all resulting finger trees into one.
    let collect mapping = Seq.map mapping >> Seq.fold concat empty

    /// Split a list where a predicate becomes true.
    let rec splitList pred (start:IMonoid<_>) = function
        | [] -> invalidIndex Messages.splitPointNotFound
        | (x::xs) as list ->
            let start = start.Add(fmeasure x)
            if pred start then
                [], list
            else
                let before, after = splitList pred start xs
                x::before, after

    /// Create a finger tree of a list with at most 4 elements.
    let chunkToTree list =
        if List.isEmpty list then Empty else
        list |> Digit.ofList |> Digit.promote

    /// Finger tree construcor the can handle empty prefixes or suffixes (given as lists).
    let rec deep prefix deeper suffix =
        match prefix, suffix with
        | [], [] ->
            match viewl deeper with
            | Nil -> Empty
            | View(head, Lazy tail) ->
                deep (Node.toList head) tail []
        | _, [] ->
            match viewr deeper with
            | Nil -> prefix |> chunkToTree
            | View (last, Lazy butLast) ->
                deep prefix butLast (Node.toList last)
        | [], _ ->
            match viewl deeper with
            | Nil -> suffix |> chunkToTree
            | View(head, Lazy tail) ->
                deep (Node.toList head) tail suffix
        | _ ->
            if prefix.Length > Digit.max || suffix.Length > Digit.max then
                invalidOp Messages.digitsCannotBeLongerThanFour
            else
                let v = (mconcat prefix).Add(fmeasure deeper).Add(mconcat suffix)
                Deep(v, Digit.ofList prefix, Lazy.CreateFromValue deeper, Digit.ofList suffix)

    /// Split a finger tree where a predicate becomes true.
    let rec split<'m, 'a
        when 'm :> IMonoid<'m>
            and 'm : (new : unit -> 'm)
            and 'a :> IMeasured<'m, 'a>
        > (pred:'m -> bool) (start:'m) : FingerTree<'m, 'a> -> Split<'m, 'a> = function
        | Empty -> invalidArg "" Messages.treeIsEmpty
        | Single x when start.Add(fmeasure x) |> pred ->
            Split(Empty, x, Empty)
        | Deep(total, prefix, deeper, suffix) when start.Add total |> pred ->
            let startPref = start.Add(fmeasure prefix)
            let startPrefDeeper = startPref.Add(fmeasure deeper.Value)

            let prefix = Digit.toList prefix
            let suffix = Digit.toList suffix

            if pred startPref then
                let before, x::after = splitList pred start prefix
                Split(chunkToTree before, x, deep after deeper.Value suffix)
            elif startPrefDeeper |> pred then
                let (Split(before, node, after)) = split pred startPref deeper.Value
                let beforeNode, x::afterNode = splitList pred <| startPref.Add(fmeasure before) <| Node.toList node
                Split(deep prefix before beforeNode, x, deep afterNode after suffix)
            else
                assert(start.Add(mconcat suffix) |> pred)
                let before, x::after = splitList pred startPrefDeeper suffix
                Split(deep prefix deeper.Value before, x, chunkToTree after)
        | _ ->
            invalidIndex Messages.splitPointNotFound
