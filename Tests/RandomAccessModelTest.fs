module CmdQ.Tests.RandomAccessModelTest

open CmdQ.FingerTree
open Fuchu
open FsCheck
open FsCheck.Experimental
open System.Diagnostics

type TestType = int16
type ModelType = ResizeArray<TestType>
type SutType = CmdQ.FingerTree.RandomAccess.Tree<TestType> ref

[<AbstractClass>]
type OpType() =
    inherit Operation<SutType, ModelType>()

    abstract member DoCheck : SutType * ModelType -> Property
    abstract member DoRun : ModelType -> unit

    override me.Run model =
        let copy = ResizeArray model
        me.DoRun copy
        copy

    override me.Check(sut, model) =
        let re = me.DoCheck(sut, model)
        !sut |> RandomAccess.depthCheck |> ignore
        re

module RandomAccess =
    let sequenceEqual seq tree =
        Seq.zip (RandomAccess.toSeq tree) seq
        |> Seq.forall (fun ab -> ab ||> (=))

type DebugMachine() as this =
    inherit Machine<SutType, ModelType>()

    let check sut model op changer delta =
        sut := !sut |> changer delta
        !sut |> RandomAccess.sequenceEqual model |@ sprintf "%s: %A" op delta

    let prepend what =
        { new OpType() with
            override __.DoRun model =
                model.Insert(0, what)

            override __.DoCheck(sut, model) = check sut model "prepend" RandomAccess.prepend what

            override __.ToString () = sprintf "prepend %A" what
        }

    let append what =
        { new OpType() with
            override __.DoRun model =
                model.Add what

            override __.DoCheck(sut, model) = check sut model "append" RandomAccess.append what

            override __.ToString () = sprintf "append %A" what
        }

    let concatRight what =
        { new OpType() with
            override __.DoRun model =
                model.AddRange what

            override __.DoCheck(sut, model) =
                let right = RandomAccess.ofList what
                sut := RandomAccess.concat !sut right
                !sut |> RandomAccess.sequenceEqual model
                |> Prop.trivial (what.Length = 0)

            override __.ToString () = sprintf "concatRight %A" what
        }

    let concatLeft what =
        { new OpType() with
            override __.DoRun model =
                model.InsertRange(0, what)

            override __.DoCheck(sut, model) =
                let left = RandomAccess.ofList what
                sut := RandomAccess.concat left !sut
                !sut |> RandomAccess.sequenceEqual model
                |> Prop.trivial (what.Length = 0)

            override __.ToString () = sprintf "concatLeft %A" what
        }

    let replace where what =
        { new OpType() with
            override __.DoRun model =
                model.[where] <- what

            override __.Pre model =
                where >= 0 && where < model.Count

            override __.DoCheck(sut, model) =
                let before = RandomAccess.get !sut where
                sut := RandomAccess.set !sut where what
                !sut |> RandomAccess.sequenceEqual model
                |> Prop.trivial (what = before)

            override __.ToString () = sprintf "replace .[%i] := %A" where what
        }

    let remove where _ =
        { new OpType() with
            override __.DoRun model =
                model.RemoveAt where

            override __.Pre model =
                where >= 0 && where < model.Count

            override me.DoCheck(sut, model) =
                sut := RandomAccess.removeIndex where !sut
                !sut |> RandomAccess.sequenceEqual model
                |@ me.ToString()

            override __.ToString () = sprintf "remove .[%i]" where
        }

    let insert where what =
        { new OpType() with
            override __.DoRun model =
                model.Insert(where, what)

            override __.Pre model =
                where >= 0 && where <= model.Count

            override me.DoCheck(sut, model) =
                sut := RandomAccess.insertAt where what !sut
                !sut |> RandomAccess.sequenceEqual model
                |@ me.ToString()

            override __.ToString () = sprintf "insert .[%i] <- %A" where what
        }

    let tail =
        { new OpType() with
            override __.DoRun model =
                model.RemoveAt 0

            override __.Pre model =
                model.Count > 0

            override me.DoCheck (sut, model)=
                sut := !sut |> RandomAccess.tail
                !sut |> RandomAccess.sequenceEqual model
                |@ me.ToString()

            override __.ToString () = "tail"
        }

    let spine =
        { new OpType() with
            override __.DoRun model =
                model.RemoveAt(model.Count - 1)

            override __.Pre model =
                model.Count > 0

            override me.DoCheck (sut, model)=
                sut := !sut |> RandomAccess.butLast
                !sut |> RandomAccess.sequenceEqual model
                |@ me.ToString()

            override __.ToString () = "spine"
        }

    let collectAddEnd (f:_ -> #seq<TestType>) what =
        { new OpType() with
            override __.DoRun model =
                let transformed = what |> Seq.collect f
                model.AddRange transformed

            override __.DoCheck(sut, model) =
                let add = what |> RandomAccess.collect (f >> RandomAccess.ofSeq)
                sut := RandomAccess.concat !sut add
                !sut |> RandomAccess.sequenceEqual model
                |> Prop.trivial (Seq.isEmpty what)

            override __.ToString() = sprintf "collectAddEnd %A" what
        }

    let collectReplace (f:_ -> #seq<TestType>) what =
        { new OpType() with
            override __.DoRun model =
                let transformed = what |> Seq.collect f
                model.Clear()
                model.AddRange transformed

            override __.DoCheck(sut, model) =
                sut := what |> RandomAccess.collect (f >> RandomAccess.ofSeq)
                !sut |> RandomAccess.sequenceEqual model
                |> Prop.trivial (Seq.isEmpty what)

            override __.ToString () = sprintf "collectReplace %A" what
        }

    let noop =
        { new OpType() with
            override __.DoRun _ = ()
            override __.DoCheck(_, _) = true |> Prop.trivial true
            override __.ToString () = "noop"
        }

    let create (initial) =
        { new Setup<SutType, ModelType>() with
            override __.Actual () = ref (RandomAccess.ofArray initial)

            override __.Model () = initial |> ResizeArray
        }

    let rndNum = Arb.from<TestType> |> Arb.toGen

    let mutable debug = false

    do
        this.InDebug()

    [<Conditional("DEBUG")>]
    member __.InDebug() =
        debug <- true

    override __.Setup =
        rndNum
        |> Gen.arrayOf
        |> Gen.map create
        |> Arb.fromGen

    override __.Next _ =
        let withElement = gen {
            let! cmd = Gen.elements [append; prepend]
            let! num = rndNum
            return cmd num
        }
        let withPositionAsPercentAndElement = gen {
            let! cmd = Gen.elements [replace; remove; insert]
            let! percent = Gen.choose(0, 101)
            let! elm = rndNum
            return cmd percent elm
        }
        let withList = gen {
            let! cmd = Gen.elements [concatLeft; concatRight]
            let! list = Gen.listOf rndNum
            return cmd list
        }
        let withUnit = gen {
            return! Gen.elements [tail; spine]
        }
        let forCollect = gen {
            if debug then
                // Otherwise we get stack overflows.
                return noop
            else
                let! cmd = Gen.elements [collectAddEnd; collectReplace]
                let! f = Arb.from<TestType -> TestType list> |> Arb.toGen
                let! list = Gen.listOf rndNum
                return cmd f list
        }
        Gen.frequency [3, withUnit; 3, withElement; 3, withPositionAsPercentAndElement; 2, withList; 1, forCollect]
        |> Gen.map (fun g -> upcast g)

let concatDequeSpec = DebugMachine()

[<Tests>]
let modelTests =
    [concatDequeSpec]
    |> List.map (StateMachine.toProperty >> testProperty "RandomAccess")
    |> testList "Model tests"
