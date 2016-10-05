namespace CmdQ
  type private Node<'a> =
    | Node2 of 'a * 'a
    | Node3 of 'a * 'a * 'a
  type private Digit<'a> =
    | One of 'a
    | Two of 'a * 'a
    | Three of 'a * 'a * 'a
    | Four of 'a * 'a * 'a * 'a
  [<NoComparisonAttribute (); NoEqualityAttribute ()>]
  type FingerTree<'a> =
    private | Empty
            | Single of 'a
            | Deep of Digit<'a> * Lazy<FingerTree<Node<'a>>> * Digit<'a>
  module Finger = begin
    val empty : FingerTree<'a>
    val isEmpty : tree:FingerTree<'a> -> bool
    val head : tree:FingerTree<'a> -> 'a
    val last : tree:FingerTree<'a> -> 'a
    val tail : tree:FingerTree<'a> -> FingerTree<'a>
    val append : z:'a -> tree:FingerTree<'a> -> FingerTree<'a>
    val prepend : a:'a -> tree:FingerTree<'a> -> FingerTree<'a>
    val concat : left:FingerTree<'a> -> right:FingerTree<'a> -> FingerTree<'a>
    val collect : mapping:('a -> FingerTree<'b>) -> (seq<'a> -> FingerTree<'b>)
    val ofList<'a> : list:'a list -> FingerTree<'a>
    val ofArray : array:'a [] -> FingerTree<'a>
    val ofSeq<'a> : seq:seq<'a> -> FingerTree<'a>
    val toSeq : tree:FingerTree<'a> -> seq<'a>
    val toArray<'a> : (FingerTree<'a> -> 'a [])
    val toList : tree:FingerTree<'a> -> 'a list
  end
