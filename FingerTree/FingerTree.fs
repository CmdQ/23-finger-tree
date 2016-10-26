namespace CmdQ

module Errors =
    let treeIsEmpty = "The input tree was empty."
    let digitAlreadyHas4Entries = "Cannot add to digit, because it already has maximum capacity."
    let onlyList2or3Accepted = "Only lists of length 2 or 3 accepted!"
    let onlyList1to4Accepted = "Only lists of length 1 to 4 accepted!"
    let patternMatchImpossible = "Impossible!"

type Node<'a> =
    | Node2 of 'a * 'a
    | Node3 of 'a * 'a * 'a

module Node =
    let ofList = function
        | [a; b] -> Node2(a, b)
        | [a; b; c] -> Node3(a, b, c)
        | _ -> failwith Errors.onlyList2or3Accepted

    let toList = function
        | Node2(a, b) -> [a; b]
        | Node3(a, b, c) -> [a; b; c]

    let rec toNodeList = function
        | []
        | [_] -> invalidOp "No enough elements."
        | [x; y] -> [Node2(x, y)]
        | [x; y; z] -> [Node3(x, y, z)]
        | x::y::rest -> Node2(x, y)::(toNodeList rest)

type Digit<'a> =
    | One of 'a
    | Two of 'a * 'a
    | Three of 'a * 'a * 'a
    | Four of 'a * 'a * 'a * 'a

[<NoComparison>]
[<NoEquality>]
type FingerTree<'a> =
    | Empty
    | Single of 'a
    | Deep of Digit<'a> * Lazy<FingerTree<Node<'a>>> * Digit<'a>

module Digit =
    let ofList = function
        | [a] -> One(a)
        | [a; b] -> Two(a, b)
        | [a; b; c] -> Three(a, b, c)
        | [a; b; c; d] -> Four(a, b, c, d)
        | _ -> failwith Errors.onlyList1to4Accepted

    let toList = function
        | One a -> [a]
        | Two(a, b) -> [a; b]
        | Three(a, b, c) -> [a; b; c]
        | Four(a, b, c, d) -> [a; b; c; d]

    let append x = function
        | One a -> Two(a, x)
        | Two(a, b) -> Three(a, b, x)
        | Three(a, b, c) -> Four(a, b, c, x)
        | _ -> failwith Errors.digitAlreadyHas4Entries

    let prepend x = function
        | One a -> Two(x, a)
        | Two(a, b) -> Three(x, a, b)
        | Three(a, b, c) -> Four(x, a, b, c)
        | _ -> failwith Errors.digitAlreadyHas4Entries

    let promote = function
        | One a -> Single a
        | Two(a, b) -> Deep(One a, Lazy.CreateFromValue Empty, One b)
        | Three(a, b, c) -> Deep(Two(a, b), Lazy.CreateFromValue Empty, One(c))
        | Four(a, b, c, d) -> Deep(Two(a, b), Lazy.CreateFromValue Empty, Two(c, d))

type View<'a> = Nil | View of 'a * Lazy<FingerTree<'a>>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FingerTree =
    let (|SplitFirst|_|) = function
        | Two(a, b) -> Some(a, One b)
        | Three(a, b, c) -> Some(a, Two(b, c))
        | Four(a, b, c, d) -> Some(a, Three(b, c, d))
        | _ -> None

    let (|SplitLast|_|) = function
        | Two(a, b) -> Some(One a, b)
        | Three(a, b, c) -> Some(Two(a, b), c)
        | Four(a, b, c, d) -> Some(Three(a, b, c), d)
        | _ -> None

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
        | Deep(SplitFirst(x, shorter), deeper, suffix) ->
            View(x, lazy Deep(shorter, deeper, suffix))
        | _ -> failwith Errors.patternMatchImpossible

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
        | Deep(prefix, deeper, SplitLast(shorter, x)) ->
            View(x, lazy Deep(prefix, deeper, shorter))
        | _ -> failwith Errors.patternMatchImpossible

    let empty = Empty

    let isEmpty tree =
        match viewl tree with
        | Nil -> true
        | _ -> false

    let head tree =
        match viewl tree with
        | View(h, _) -> h
        | _ -> invalidArg "tree" Errors.treeIsEmpty

    let tail tree =
        match viewl tree with
        | View(_, Lazy t) -> t
        | _ -> invalidArg "tree" Errors.treeIsEmpty

    let last tree =
        match viewr tree with
        | View(h, _) -> h
        | _ -> invalidArg "tree" Errors.treeIsEmpty

    let rec append<'a> (z:'a) : FingerTree<'a> -> FingerTree<'a> = function
        | Empty -> Single z
        | Single y -> Deep(One y, Lazy.CreateFromValue Empty, One z)
        | Deep(prefix, Lazy deeper, Four(v, w, x, y)) ->
            // Force evaluation here, because the dept has already been paid for.
            Deep(prefix, Lazy.CreateFromValue(append (Node3(v, w, x)) deeper), Two(y, z))
        | Deep(prefix, deeper, suffix) ->
            Deep(prefix, deeper, suffix |> Digit.append z)

    let rec prepend<'a> (a:'a) : FingerTree<'a> -> FingerTree<'a> = function
        | Empty -> Single a
        | Single b -> Deep(One a, Lazy.CreateFromValue Empty, One b)
        | Deep(Four(b, c, d, e), Lazy deeper, suffix) ->
            // Force evaluation here, because the dept has already been paid for.
            Deep(Two(a, b), Lazy.CreateFromValue(prepend (Node3(c, d, e)) deeper), suffix)
        | Deep(prefix, deeper, suffix) ->
            Deep(prefix |> Digit.prepend a, deeper, suffix)

    let ofList list = List.fold (flip append<'a>) empty list

    let ofArray arr = Array.foldBack prepend arr empty

    let ofSeq seq = Seq.fold (flip append<'a>) empty seq

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

    let toArray<'a> = toSeq<'a> >> Seq.toArray

    let rec toList tree =
        match viewl tree with
        | Nil -> []
        | View(head, Lazy tail) -> head::(toList tail)

    // http://andrew.gibiansky.com/blog/haskell/finger-trees/#Concatenation
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
        | _ -> failwith Errors.patternMatchImpossible

    let concat left right = concatWithMiddle(left, [], right)

    let collect mapping = Seq.map mapping >> Seq.fold concat empty