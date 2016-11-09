module CmdQ.Tests.RandomAccessModelTest

open CmdQ.FingerTree
open Fuchu
open FsCheck
open FsCheck.Experimental

type TestType = string
type ModelType = ResizeArray<TestType>
type SutType = CmdQ.FingerTree.RandomAccess.Tree<TestType> ref
type OpType = Operation<SutType, ModelType>

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
            override __.Run model =
                let copy = model |> ResizeArray
                copy.Insert(0, what)
                copy

            override __.Check(sut, model) = check sut model "prepend" RandomAccess.prepend what

            override __.ToString () = sprintf "prepend %A" what
        }

    let append what =
        { new OpType() with
            override __.Run model =
                let copy = model |> ResizeArray
                copy.Add what
                copy

            override __.Check(sut, model) = check sut model "append" RandomAccess.append what

            override __.ToString () = sprintf "append %A" what
        }

    let concatRight what =
        { new OpType() with
            override __.Run model =
                let copy = model |> ResizeArray
                copy.AddRange what
                copy

            override __.Check(sut, model) =
                let right = RandomAccess.ofList what
                sut := RandomAccess.concat !sut right
                !sut |> RandomAccess.sequenceEqual model
                |> Prop.trivial (what.Length = 0)

            override __.ToString () = sprintf "concatRight %A" what
        }

    let concatLeft what =
        { new OpType() with
            override __.Run model =
                let copy = model |> ResizeArray
                copy.InsertRange(0, what)
                copy

            override __.Check(sut, model) =
                let left = RandomAccess.ofList what
                sut := RandomAccess.concat left !sut
                !sut |> RandomAccess.sequenceEqual model
                |> Prop.trivial (what.Length = 0)

            override __.ToString () = sprintf "concatLeft %A" what
        }

    let collectAddEnd (f:_ -> #seq<TestType>) what =
        { new OpType() with
            override __.Run model =
                let transformed = what |> Seq.collect f
                let copy = model |> ResizeArray
                copy.AddRange transformed
                copy

            override __.Check(sut, model) =
                let add = what |> RandomAccess.collect (f >> RandomAccess.ofSeq)
                sut := RandomAccess.concat !sut add
                !sut |> RandomAccess.sequenceEqual model
                |> Prop.trivial (Seq.isEmpty what)

            override __.ToString() = sprintf "collectAddEnd %A" what
        }

    let collectReplace (f:_ -> #seq<TestType>) what =
        { new OpType() with
            override __.Run model =
                let transformed = what |> Seq.collect f
                ResizeArray(transformed)

            override __.Check(sut, model) =
                sut := what |> RandomAccess.collect (f >> RandomAccess.ofSeq)
                !sut |> RandomAccess.sequenceEqual model
                |> Prop.trivial (Seq.isEmpty what)

            override __.ToString() = sprintf "collectReplace %A" what
        }

    let tail =
        { new OpType() with
            override __.Run model =
                let copy = model |> ResizeArray
                copy.RemoveAt 0
                copy

            override __.Pre model =
                model.Count > 0

            override me.Check (sut, model)=
                sut := !sut |> RandomAccess.tail
                !sut |> RandomAccess.sequenceEqual model
                |@ me.ToString()

            override __.ToString () = "tail"
        }

    let spine =
        { new OpType() with
            override __.Run model =
                let copy = model |> ResizeArray
                copy.RemoveAt(copy.Count - 1)
                copy

            override __.Pre model =
                model.Count > 0

            override me.Check (sut, model)=
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
            let withList = gen {
                let! cmd = Gen.elements [concatLeft; concatRight]
                let! list = Gen.listOf rndNum
                return cmd list
            }
            let withUnit = gen {
                return! Gen.elements [tail; spine]
            }
            let forCollect = gen {
                let! cmd = Gen.elements [collectAddEnd; collectReplace]
                let! f = Arb.from<TestType -> TestType list> |> Arb.toGen
                let! list = Gen.listOf rndNum
                return cmd f list
            }
            Gen.oneof [withUnit; withElement; withList; forCollect]
    }

[<Tests>]
let modelTests =
    [concatDequeSpec]
    |> List.map (StateMachine.toProperty >> testProperty "RandomAccess")
    |> testList "Model tests"
