module CmdQ.FingerTree.Tests.MyArbitraries

open FsCheck

type PosInt = PosInt of int with static member op_Explicit (PosInt i) = i

type NegInt = NegInt of int with static member op_Explicit (NegInt i) = i

let ge0 = flip (>=) 0

let posInt () =
    Arb.from<int>
    |> Arb.filter ge0
    |> Arb.convert PosInt int

let negInt () =
    Arb.from<int>
    |> Arb.filter (ge0 >> not)
    |> Arb.convert NegInt int
