module CmdQ.Tests.RandomAccessModelTest

open CmdQ.FingerTree
open Fuchu
open FsCheck
open FsCheck.Experimental

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

let concatDequeSpec =
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

            override __.ToString () = sprintf "replace .[%i] <- %A" where what
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

    let create (initial) =
        { new Setup<SutType, ModelType>() with
            override __.Actual () = ref (RandomAccess.ofArray initial)

            override __.Model () = initial |> ResizeArray
        }

    let rndNum = Arb.from<TestType> |> Arb.toGen

    { new Machine<SutType, ModelType>() with
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
                let! percent = Gen.choose(0, 101)
                let! elm = rndNum
                return replace percent elm
            }
            let withList = gen {
                let! cmd = Gen.elements [concatLeft; concatRight]
                let! list = Gen.listOf rndNum
                return cmd list
            }
            let withUnit = gen {
                return! Gen.elements [tail; spine]
            }
            Gen.frequency [3, withUnit; 3, withElement; 3, withPositionAsPercentAndElement; 1, withList]
            |> Gen.map (fun g -> upcast g)
    }

[<Tests>]
let modelTests =
    [concatDequeSpec]
    |> List.map (StateMachine.toProperty >> testProperty "RandomAccess")
    |> testList "Model tests"
