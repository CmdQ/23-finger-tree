﻿namespace CmdQ.FingerTree.Monoidal

#nowarn "25"

open CmdQ.FingerTree.Error
open CmdQ.FingerTree.Monoids
open System
open System.Diagnostics

/// A 2-3-Node can hold either 2 or 3 elements.
type Node<'m, 'a when 'm :> IMonoid<'m>> =
    | Node2 of 'm * 'a * 'a
    | Node3 of 'm * 'a * 'a * 'a

    interface IMeasured<'m> with
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
        | x::y::rest -> Node2(mconcat [x; y], x, y)::(toNodeList rest)

    /// Apply a function while also reversing the results.
    let revMap f = function
        | Node2(v, a, b)    -> Node2(v, f b, f a)
        | Node3(v, a, b, c) -> Node3(v, f c, f b, f a)

/// A digit holds at least 1 and up to 4 elements.
type Digit<'m, 'a
        when 'm :> IMonoid<'m>
        and 'a :> IMeasured<'m>
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

    interface IMeasured<'m> with
        member me.Measure = me.ToList() |> mconcat

/// A finger tree is either empty, holds a single elements or gets recursive with a prefix and a suffix of digits.
[<NoComparison>]
[<NoEquality>]
[<DebuggerDisplay("{DebuggerDisplay,nq}", Type = "{TypeName}")>]
type FingerTree<'m, 'a
        when 'a :> IMeasured<'m>
        and 'm :> IMonoid<'m>
        and 'm : (new : unit -> 'm)
    > =
    | Empty
    | Single of 'a
    | Deep of 'm * Digit<'m, 'a> * Lazy<FingerTree<'m, Node<'m, 'a>>> * Digit<'m, 'a>

    member __.Monoid = Singleton.Instance

    interface IMeasured<'m> with
        member me.Measure =
            match me with
            | Empty ->
                me.Monoid
            | Single x -> fmeasure x
            | Deep(v, _, _, _) -> v

    static member private TypeName =
        CmdQ.FingerTree.FingerTree<_>.TypeName

    member private me.DebuggerDisplay =
        match me with
        | Deep(v, prefix, Lazy Empty, suffix) ->
            sprintf "%s ( [%A] %A | %A )" CmdQ.FingerTree.FingerTree<_>.DeepName v (prefix.ToList()) (suffix.ToList())
        | Deep(v, prefix, Lazy (Single _), suffix) ->
            sprintf "%s ( [%A] %A | . | %A )" CmdQ.FingerTree.FingerTree<_>.DeepName v (prefix.ToList()) (suffix.ToList())
        | Deep(v, prefix, deeper, suffix) ->
            deeper.Force() |> ignore
            sprintf "%s ( [%A] %A | … | %A )" CmdQ.FingerTree.FingerTree<_>.DeepName v (prefix.ToList()) (suffix.ToList())
        | _ -> sprintf "%A" me

    override me.ToString () = me.DebuggerDisplay

module Digit =
    /// Convert a list of values to a digit.
    let ofList = function
        | [a] -> One(a)
        | [a; b] -> Two(a, b)
        | [a; b; c] -> Three(a, b, c)
        | [a; b; c; d] -> Four(a, b, c, d)
        | _ -> invalidOp Messages.onlyList1to4Accepted

    /// Directly convert a node to a digit.
    let ofNode = function
        | Node2(_, a, b) -> Two(a, b)
        | Node3(_, a, b, c) -> Three(a, b, c)

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
        | Two(a, b) -> Deep(mconcat [a; b], One a, lazyval Empty, One b)
        | Three(a, b, c) -> Deep(mconcat [a; b; c], Two(a, b), lazyval Empty, One(c))
        | Four(a, b, c, d) -> Deep(mconcat [a; b; c; d], Two(a, b), lazyval Empty, Two(c, d))

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

    /// Apply a function while also reversing the results.
    let revMap f = function
        | One   a           -> One  (               f a)
        | Two  (a, b)       -> Two  (          f b, f a)
        | Three(a, b, c)    -> Three(     f c, f b, f a)
        | Four (a, b, c, d) -> Four (f d, f c, f b, f a)

/// A View is either empty or points to an element in the finger tree and its position.
type View<'m, 'a
        when 'a :> IMeasured<'m>
        and 'm :> IMonoid<'m>
        and 'm : (new : unit -> 'm)
    > = Nil | View of 'a * Lazy<FingerTree<'m, 'a>>

/// A split points to an element in the finger tree and also provides the two finger trees around that element.
type Split<'m, 'a
        when 'a :> IMeasured<'m>
        and 'm :> IMonoid<'m>
        and 'm : (new : unit -> 'm)
    > = Split of FingerTree<'m, 'a> * 'a * FingerTree<'m, 'a>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FingerTree =
    module Basic = CmdQ.FingerTree.ConcatDeque

    /// Return both the left-most element and the remaining tree (lazily).
    let rec viewl<'m, 'a
            when 'a :> IMeasured<'m>
            and 'm : (new : unit -> 'm)
        > : FingerTree<'m, 'a> -> View<'m, 'a> = function
        | Empty -> Nil
        | Single x -> View(x, lazyval Empty)
        | Deep(_, One x, deeper, suffix) ->
            let rest = lazy (
                match viewl deeper.Value with
                | Nil ->
                    suffix |> Digit.promote
                | View (node, lazyRest) ->
                    let v = (fmeasure node).Plus(fmeasure lazyRest.Value).Plus(fmeasure suffix)
                    Deep(v, node |> Digit.ofNode, lazyRest, suffix)
            )
            View(x, rest)
        | Deep(_, Digit.SplitFirst(x, shorter), deeper, suffix) ->
            let lazyRest = lazy (
                let v = (fmeasure shorter).Plus(fmeasure deeper.Value).Plus(fmeasure suffix)
                Deep(v, shorter, deeper, suffix)
            )
            View(x, lazyRest)

    /// Return both the right-most element and the remaining tree (lazily).
    let rec viewr<'m, 'a
            when 'a :> IMeasured<'m>
            and 'm : (new : unit -> 'm)
        > : FingerTree<'m, 'a> -> View<'m, 'a> = function
        | Empty -> Nil
        | Single x -> View(x, lazyval Empty)
        | Deep(_, prefix, deeper, One x) ->
            let rest = lazy (
                match viewr deeper.Value with
                | Nil ->
                    prefix |> Digit.promote
                | View (node, lazyRest) ->
                    let v = (fmeasure prefix).Plus(fmeasure lazyRest.Value).Plus(fmeasure node)
                    Deep(v, prefix, lazyRest, node |> Digit.ofNode)
            )
            View(x, rest)
        | Deep(_, prefix, deeper, Digit.SplitLast(shorter, x)) ->
            let lazyRest = lazy (
                let v = (fmeasure prefix).Plus(fmeasure deeper.Value).Plus(fmeasure shorter)
                Deep(v, prefix, deeper, shorter)
            )
            View(x, lazyRest)

    /// Append an element to the right of a tree.
    let rec append<'m, 'a
            when 'a :> IMeasured<'m>
            and 'm : (new : unit -> 'm)
        > (z:'a) : FingerTree<'m, 'a> -> FingerTree<'m, 'a> = function
        | Empty -> Single z
        | Single y ->
            Deep(mconcat [y; z], One y, lazyval Empty, One z)
        | Deep(total, prefix, Lazy deeper, Four(v, w, x, y)) ->
            // Force evaluation here, because the dept has already been paid for.
            Deep(total.Plus (fmeasure z),
                prefix,
                lazyval(append (Node.ofList [v; w; x]) deeper),
                Two(y, z))
        | Deep(total, prefix, deeper, suffix) ->
            Deep(total.Plus (fmeasure z), prefix, deeper, suffix |> Digit.append z)

    /// Prepend an element to the left of a tree.
    let rec prepend<'m, 'a
            when 'a :> IMeasured<'m>
            and 'm : (new : unit -> 'm)
        > (a:'a) : FingerTree<'m, 'a> -> FingerTree<'m, 'a> = function
        | Empty -> Single a
        | Single b ->
            Deep(mconcat [a; b], One a, lazyval Empty, One b)
        | Deep(total, Four(b, c, d, e), Lazy deeper, suffix) ->
            // Force evaluation here, because the dept has already been paid for.
            Deep((fmeasure a).Plus total,
                Two(a, b),
                lazyval(prepend (Node.ofList [c; d; e]) deeper),
                suffix)
        | Deep(total, prefix, deeper, suffix) ->
            Deep((fmeasure a).Plus total, prefix |> Digit.prepend a, deeper, suffix)

    /// Concatenate two trees while putting a list of elements in the middle.
    let rec
        concatWithMiddle<'m, 'a
                when 'a :> IMeasured<'m>
                and 'm : (new : unit -> 'm)
        > : FingerTree<'m, 'a> * 'a list * FingerTree<'m, 'a> -> FingerTree<'m, 'a> = function
        | Empty, [], only
        | only, [], Empty -> only
        | Empty, left::rest, right
        | Single left, rest, right -> concatWithMiddle(Empty, rest, right) |> prepend left
        | left, List.Snoc(rest, right), Empty
        | left, rest, Single right -> concatWithMiddle(left, rest, Empty) |> append right
        | Deep(leftTotal, leftPrefix, leftDeeper, leftSuffix), middle, Deep(rightTotal, rightPrefix, rightDeeper, rightSuffix) ->
            let deeper = lazy (
                let left = leftSuffix |> Digit.toList
                let right = rightPrefix |> Digit.toList
                let middle = List.concat [left; middle; right] |> Node.toNodeList
                concatWithMiddle(leftDeeper.Value, middle, rightDeeper.Value)
            )
            let v =
                if List.isEmpty middle then
                    leftTotal.Plus rightTotal
                else
                    leftTotal.Plus(mconcat middle).Plus rightTotal
            Deep(v, leftPrefix, deeper, rightSuffix)

    /// Concatenate two finger trees.
    let concat left right = concatWithMiddle(left, [], right)

    /// Split a list where a predicate becomes true.
    let rec splitList pred (start:IMonoid<_>) = function
        | [] -> invalidIndex Messages.splitPointNotFound
        | (x::xs) as list ->
            assert(list.Length <= CmdQ.FingerTree.Digit.max)
            let start = start.Plus(fmeasure x)
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
            if prefix.Length > CmdQ.FingerTree.Digit.max || suffix.Length > CmdQ.FingerTree.Digit.max then
                invalidOp Messages.digitsCannotBeLongerThanFour
            else
                let v = (mconcat prefix).Plus(fmeasure deeper).Plus(mconcat suffix)
                Deep(v, Digit.ofList prefix, lazyval deeper, Digit.ofList suffix)

    /// Split a finger tree where a predicate becomes true.
    let rec split<'m, 'a
            when 'a :> IMeasured<'m>
            and 'm : (new : unit -> 'm)
        > (pred:'m -> bool) (start:'m) : FingerTree<'m, 'a> -> Split<'m, 'a> = function
        | Empty ->
            invalidArg "" Messages.treeIsEmpty
        | Single x when start.Plus(fmeasure x) |> pred ->
            Split(Empty, x, Empty)
        | Deep(total, prefix, deeper, suffix) when start.Plus total |> pred ->
            let startPref = start.Plus(fmeasure prefix)
            let startPrefDeeper = startPref.Plus(fmeasure deeper.Value)

            let prefix = Digit.toList prefix
            let suffix = Digit.toList suffix

            if pred startPref then
                let before, x::after = splitList pred start prefix
                Split(chunkToTree before, x, deep after deeper.Value suffix)
            elif startPrefDeeper |> pred then
                let (Split(before, node, after)) = split pred startPref deeper.Value
                let beforeNode, x::afterNode = splitList pred <| startPref.Plus(fmeasure before) <| Node.toList node
                Split(deep prefix before beforeNode, x, deep afterNode after suffix)
            else
                let before, x::after = splitList pred startPrefDeeper suffix
                Split(deep prefix deeper.Value before, x, chunkToTree after)
        | _ ->
            invalidIndex Messages.splitPointNotFound

    let rec private rev'<'m, 'a when 'a :> IMeasured<'m> and 'm : (new : unit -> 'm)>
        (f:'a -> 'a) : tree:FingerTree<'m, 'a> -> FingerTree<'m, 'a>
        = function
        | Empty as tree                       -> tree
        | Single x                            -> Single(f x)
        | Deep(total, prefix, deeper, suffix) ->
            Deep(total, Digit.revMap f suffix, lazy (rev' (Node.revMap f) deeper.Value), Digit.revMap f prefix)

    /// Reverse the content of a tree.
    let rev tree = rev' id tree
