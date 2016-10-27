namespace CmdQ

open Monoid

type Node<'m, 'a when 'm :> IMonoid<'m>> =
    | Node2 of 'm * 'a * 'a
    | Node3 of 'm * 'a * 'a * 'a

    interface IMeasured<'m, Node<'m, 'a>> with
        member me.Measure =
            match me with
            | Node2(v, _, _) -> v
            | Node3(v, _, _, _) -> v

module Node =
    let ofList list =
        match list with
        | [a; b] -> Node2(mconcat list, a, b)
        | [a; b; c] -> Node3(mconcat list, a, b, c)
        | _ -> failwith ErrorMessages.onlyList2or3Accepted

    let toList = function
        | Node2(_, a, b) -> [a; b]
        | Node3(_, a, b, c) -> [a; b; c]

    let rec toNodeList list =
        match list with
        | []
        | [_] -> invalidOp "No enough elements."
        | [x; y] -> [Node2(mconcat list, x, y)]
        | [x; y; z] -> [Node3(mconcat list, x, y, z)]
        | x::y::rest -> Node2(mconcat list, x, y)::(toNodeList rest)

type Digit<'m, 'a
    when 'm :> IMonoid<'m>
        and 'a :> IMeasured<'m, 'a>
    > =
    | One of 'a
    | Two of 'a * 'a
    | Three of 'a * 'a * 'a
    | Four of 'a * 'a * 'a * 'a

    member me.ToList () =
        match me with
        | One a -> [a]
        | Two(a, b) -> [a; b]
        | Three(a, b, c) -> [a; b; c]
        | Four(a, b, c, d) -> [a; b; c; d]

    interface IMeasured<'m, 'a> with
        member me.Measure = me.ToList() |> mconcat

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
    let ofList = function
        | [a] -> One(a)
        | [a; b] -> Two(a, b)
        | [a; b; c] -> Three(a, b, c)
        | [a; b; c; d] -> Four(a, b, c, d)
        | _ -> failwith ErrorMessages.onlyList1to4Accepted

    let toList (digit:Digit<_, _>) = digit.ToList()

    let append x = function
        | One a -> Two(a, x)
        | Two(a, b) -> Three(a, b, x)
        | Three(a, b, c) -> Four(a, b, c, x)
        | _ -> failwith ErrorMessages.digitAlreadyHas4Entries

    let prepend x = function
        | One a -> Two(x, a)
        | Two(a, b) -> Three(x, a, b)
        | Three(a, b, c) -> Four(x, a, b, c)
        | _ -> failwith ErrorMessages.digitAlreadyHas4Entries

    let promote = function
        | One a -> Single a
        | Two(a, b) -> Deep(mconcat [a; b], One a, Lazy.CreateFromValue Empty, One b)
        | Three(a, b, c) -> Deep(mconcat [a; b; c], Two(a, b), Lazy.CreateFromValue Empty, One(c))
        | Four(a, b, c, d) -> Deep(mconcat [a; b; c; d], Two(a, b), Lazy.CreateFromValue Empty, Two(c, d))

type View<'m, 'a
    when 'm :> IMonoid<'m>
        and 'm : (new : unit -> 'm)
        and 'a :> IMeasured<'m, 'a>
    > = Nil | View of 'a * Lazy<FingerTree<'m, 'a>>

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
        | Deep(_, SplitFirst(x, shorter), deeper, suffix) ->
            let lazyRest = lazy (
                let v = (fmeasure shorter).Add(fmeasure deeper.Value).Add(fmeasure suffix)
                Deep(v, shorter, deeper, suffix)
            )
            View(x, lazyRest)
        | _ -> failwith ErrorMessages.patternMatchImpossible

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
        | Deep(_, prefix, deeper, SplitLast(shorter, x)) ->
            let lazyRest = lazy (
                let v = (fmeasure prefix).Add(fmeasure deeper.Value).Add(fmeasure shorter)
                Deep(v, prefix, deeper, shorter)
            )
            View(x, lazyRest)
        | _ -> failwith ErrorMessages.patternMatchImpossible

    let empty = Empty

    let isEmpty tree =
        match viewl tree with
        | Nil -> true
        | _ -> false

    let head tree =
        match viewl tree with
        | View(h, _) -> h
        | _ -> invalidArg "tree" ErrorMessages.treeIsEmpty

    let tail tree =
        match viewl tree with
        | View(_, Lazy t) -> t
        | _ -> invalidArg "tree" ErrorMessages.treeIsEmpty

    let last tree =
        match viewr tree with
        | View(h, _) -> h
        | _ -> invalidArg "tree" ErrorMessages.treeIsEmpty

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

    let ofList list = List.fold (flip append<'m, 'a>) empty list

    let ofArray arr = Array.foldBack prepend arr empty

    let ofSeq seq = Seq.fold (flip append<'m, 'a>) empty seq

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

    let toArray<'m, 'a
        when 'm :> IMonoid<'m>
            and 'm : (new : unit -> 'm)
            and 'a :> IMeasured<'m, 'a>
        > = toSeq<'m, 'a> >> Seq.toArray

    let rec toList tree =
        match viewl tree with
        | Nil -> []
        | View(head, Lazy tail) -> head::(toList tail)

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
        | _ -> failwith ErrorMessages.patternMatchImpossible

    let concat left right = concatWithMiddle(left, [], right)

    let collect mapping = Seq.map mapping >> Seq.fold concat empty
