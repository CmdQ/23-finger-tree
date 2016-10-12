module CmdQ.Tests.FingerTreeModelTest

open CmdQ
open Fuchu
open FsCheck
open FsCheck.Experimental

type TestType = uint16
type ModelType = ResizeArray<TestType>
type SutType = FingerTree<TestType> ref

let fingerTreeSpec =
    let check sut model changer delta =
        sut := !sut |> changer delta
        let a = !sut |> Finger.toSeq
        let areEqual =
            Seq.zip a model
            |> Seq.forall (fun ab -> ab ||> (=))
        areEqual |@ sprintf "prepend: %A" delta

    let prepend (what:TestType) =
        { new Operation<SutType, ModelType>() with
            override __.Run model =
                let copy = model |> ResizeArray
                copy.Insert(0, what)
                copy

            override __.Check(sut, model) = check sut model Finger.prepend what

            override __.ToString() = sprintf "prepend %A" what
        }

    let append (what:TestType) =
        { new Operation<SutType, ModelType>() with
            override __.Run model =
                let copy = model |> ResizeArray
                copy.Add what
                copy

            override __.Check(sut, model) = check sut model Finger.append what

            override __.ToString() = sprintf "append %A" what
        }

    let create (initial) =
        { new Setup<SutType, ModelType>() with
            override __.Actual () = ref (Finger.ofArray initial)

            override __.Model () = initial |> ResizeArray
        }

    let rndNum () = Arb.from<TestType> |> Arb.toGen

    { new Machine<SutType, ModelType>() with
        override __.Setup =
            rndNum()
            |> Gen.arrayOf
            |> Gen.map create
            |> Arb.fromGen

        override __.Next _ = gen {
            let! cmd = Gen.elements [append; prepend]
            let! num = rndNum()
            return cmd num
        }
    }

[<Tests>]
let modelTests =
    [fingerTreeSpec]
    |> List.map (StateMachine.toProperty >> testProperty "Finger tree")
    |> testList "Model tests"
