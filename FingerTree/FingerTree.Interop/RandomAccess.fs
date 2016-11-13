namespace CmdQ.FingerTree.Interop

open CmdQ.FingerTree
open CmdQ.FingerTree.Monoidal
open CmdQ.FingerTree.Monoids.RandomAccess
open System
open System.Collections.Generic
open System.Runtime.CompilerServices

type RandomAccess<'T>(source:seq<'T>) =
    let mutable tree = source |> RandomAccess.ofSeq

    new () = RandomAccess<'T>(Seq.empty:seq<'T>)

    new (source:RandomAccess<_>) as this =
        RandomAccess()
        then
            this.Tree <- source.Tree

    member internal __.Tree
        with get () = tree
        and set value = tree <- value

    member __.Head = tree |> RandomAccess.head

    member __.Last = tree |> RandomAccess.last

    member __.Tail () = RandomAccess(Tree = RandomAccess.tail tree)

    member __.ButLast () = RandomAccess(Tree = RandomAccess.butLast tree)

    member __.Concat (rhs:RandomAccess<_>) = tree <- RandomAccess.concat tree rhs.Tree

    // interface IList<'T> with

    member __.Count = tree |> RandomAccess.length

    member __.Add item = tree <- RandomAccess.append item tree

    member __.Clear () = tree <- RandomAccess.empty

    member __.Contains item = failwith "Not implemented yet"

    member me.CopyTo(array:'T [], arrayIndex) =
        if isNull array then
            ArgumentNullException("array") |> raise
        if arrayIndex < 0 then
            ArgumentOutOfRangeException("arrayIndex", "Index cannot be negative.") |> raise
        if array.Length - arrayIndex < me.Count then
            ArgumentException("The destination array does not have enough room to store everything.") |> raise
        let rec loop i tree =
            match FingerTree.viewl tree with
            | View(Value head, Lazy tail) ->
                array.[i] <- head
                loop (i + 1) tail
            | Nil ->
                assert(i = me.Count)
        loop 0 tree

    member __.GetEnumerator () =
        let s = tree |> RandomAccess.toSeq
        s.GetEnumerator()

    member __.IndexOf item = failwith "Not implemented yet"

    member __.Insert(index, item) = tree <- tree |> RandomAccess.insertAt index item

    member __.IsReadOnly = false

    member __.Item
        with get index = tree |> RandomAccess.item index
        and set index v = tree <- RandomAccess.set tree index v

    member __.Remove item = failwith "Not implemented yet"

    member __.RemoveAt index = tree <- tree |> RandomAccess.removeIndex index

    interface IList<'T> with
        member me.Add item = me.Add item
        member me.Clear () = me.Clear()
        member me.Contains item = me.Contains item
        member me.CopyTo(array, arrayIndex) = me.CopyTo(array, arrayIndex)
        member me.Count = me.Count
        member me.GetEnumerator () = me.GetEnumerator()
        member me.GetEnumerator () = me.GetEnumerator() :> System.Collections.IEnumerator
        member me.IndexOf item = me.IndexOf item
        member me.Insert(index, item) = me.Insert(index, item)
        member me.IsReadOnly = me.IsReadOnly
        member me.Item
            with get index = me.[index]
            and set index v = me.[index] <- v
        member me.Remove item = me.Remove item
        member me.RemoveAt index = me.RemoveAt index

[<Extension>]
type RandomAccess =
    static member private Construct tree = RandomAccess(Tree = tree)

    static member Emtpy<'T>() = RandomAccess<'T>()

    [<Extension>]
    static member Append(tree:RandomAccess<_>, item) =
        tree.Tree |> RandomAccess.append item |> RandomAccess.Construct

    [<Extension>]
    static member Prepend(tree:RandomAccess<_>, item) =
        tree.Tree |> RandomAccess.prepend item |> RandomAccess.Construct

    [<Extension>]
    static member Concat(tree:RandomAccess<_>, rhs:RandomAccess<_>) =
        RandomAccess.concat tree.Tree rhs.Tree |> RandomAccess.Construct

    [<Extension>]
    static member Set(tree:RandomAccess<_>, index, value) =
        RandomAccess.set tree.Tree index value |> RandomAccess.Construct
