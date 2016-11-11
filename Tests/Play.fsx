#r @"../FingerTree/bin/Debug/FingerTree.dll"
open CmdQ.FingerTree

let tree = seq {1..200000} |> ConcatDeque.ofSeq
let back = tree |> ConcatDeque.toList

let test = RandomAccess.ofList [1..20]
test |> RandomAccess.toList |> printfn "%A"

test
|> RandomAccess.insertAt 0 0
|> RandomAccess.insertAt 21 100
|> RandomAccess.insertAt 2 99
|> RandomAccess.toList
