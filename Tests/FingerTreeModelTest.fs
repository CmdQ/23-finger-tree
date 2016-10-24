module CmdQ.Tests.FingerTreeModelTest

open CmdQ
open Fuchu
open FsCheck
open FsCheck.Experimental

type TestType = uint16
type ModelType = ResizeArray<TestType>
type SutType = FingerTree<TestType> ref

let fingerTreeSpec =
    let check sut model op changer delta =
        let before = !sut |> Finger.toList
        sut := !sut |> changer delta
        let after = !sut |> Finger.toList
        let a = !sut |> Finger.toSeq
        let areEqual =
            Seq.zip a model
            |> Seq.forall (fun ab -> ab ||> (=))
        areEqual |@ sprintf "%s: %A" op delta |@ sprintf "Tree%A -> Tree%A" before after

    let prepend what =
        { new Operation<SutType, ModelType>() with
            override __.Run model =
                let copy = model |> ResizeArray
                copy.Insert(0, what)
                copy

            override __.Check(sut, model) = check sut model "prepend" Finger.prepend what

            override __.ToString () = sprintf "prepend %A" what
        }

    let append what =
        { new Operation<SutType, ModelType>() with
            override __.Run model =
                let copy = model |> ResizeArray
                copy.Add what
                copy

            override __.Check(sut, model) = check sut model "append" Finger.append what

            override __.ToString () = sprintf "append %A" what
        }

    let concat what =
        { new Operation<SutType, ModelType>() with
            override __.Run model =
                let copy = model |> ResizeArray
                copy.AddRange what
                copy

            override __.Check(sut, model) =
                let right = Finger.ofList what
                check sut model "concat" Finger.concat right

            override __.ToString () = sprintf "concat %A" what
        }

    let create (initial) =
        { new Setup<SutType, ModelType>() with
            override __.Actual () = ref (Finger.ofArray initial)

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
            let withList = gen {
                let elm = Arb.from<TestType> |> Arb.toGen
                let! list = Gen.listOf elm
                return concat list
            }
            Gen.oneof [withElement(*; withList*)]
    }

[<Tests>]
let modelTests =
    [fingerTreeSpec]
    |> List.map (StateMachine.toProperty >> testProperty "Finger tree")
    |> testList "Model tests"
