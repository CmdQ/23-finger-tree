module CmdQ.Tests.FingerTreeModelTest

open CmdQ
open Fuchu
open FsCheck
open FsCheck.Experimental

type TestType = uint16

let spec =
    let prepend (what:TestType) =
        { new Operation<FingerTree<TestType>, ResizeArray<TestType>>() with
            override __.Run m =
                m.Insert(0, what)
                m
            override __.Check(c, m) =
                let res = c |> Finger.prepend what
                let actl = res |> Finger.toList
                let modl = m |> Seq.toList
                actl = modl
                    |@ sprintf "prepend: model = %A, actual = %A" modl actl
            override __.ToString() = "prepend"
        }
    let create initial =
        { new Setup<FingerTree<TestType>, ResizeArray<TestType>>() with
            override __.Actual () = initial |> Finger.ofList
            override __.Model () = initial |> ResizeArray
        }

    let rndNum () : Gen<TestType> =
        Gen.choose(int TestType.MinValue, int TestType.MaxValue)
        |> Gen.map uint16

    { new Machine<FingerTree<TestType>, ResizeArray<TestType>>() with
        override __.Setup =
            rndNum()
            |> Gen.listOf
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
