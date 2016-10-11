module CmdQ.Tests.FingerTreeModelTest

open CmdQ
open Fuchu
open FsCheck
open FsCheck.Experimental

type TestType = uint16
type ModelType = ResizeArray<TestType>
type SutType = FingerTree<TestType>

let spec =
    let prepend (what:TestType) =
        { new Operation<SutType, ModelType>() with
            override __.Run model =
                // Also tried returning the same instance.
                let copy = model |> ResizeArray
                copy.Insert(0, what)
                copy

            override __.Check(sut, model) =
                let sutList = sut |> Finger.toList
                let newSut = sut |> Finger.prepend what
                let newSutList = newSut |> Finger.toList
                let modelList = model |> Seq.toList
                let areEqual = newSutList = modelList
                areEqual |@ sprintf "prepend: model = %A, actual = %A (incoming was %A)" modelList newSutList sutList

            override __.ToString() = sprintf "prepend %A" what
        }

    let create (initial:ModelType) =
        { new Setup<SutType, ModelType>() with
            override __.Actual () = initial |> Finger.ofSeq

            override __.Model () = initial //|> ResizeArray // Also tried this.
        }

    let rndNum () : Gen<TestType> = Arb.from<uint16> |> Arb.toGen

    { new Machine<SutType, ModelType>() with
        override __.Setup =
            rndNum()
            |> Gen.listOf
            |> Gen.map ResizeArray
            |> Gen.map create
            |> Arb.fromGen

        override __.Next _ = gen {
            let! cmd = Gen.elements [prepend]
            let! num = rndNum()
            return cmd num
        }
    }

[<Tests>]
let test =
    [spec]
    |> List.map (StateMachine.toProperty >> testProperty "Finger tree")
    |> testList "Model tests"
