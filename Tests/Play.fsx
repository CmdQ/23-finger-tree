#r @"../FingerTree/bin/Debug/FingerTree.dll"
open CmdQ.FingerTree

let test = RandomAccess.ofList [1..20]
test |> RandomAccess.toList |> printfn "%A"

RandomAccess.sub test 14 5 |> RandomAccess.toList

let t = RandomAccess.sub test 5 5
t |> RandomAccess.toList
RandomAccess.sub t 3 2 |> RandomAccess.toList
