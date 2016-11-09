#r @"..\FingerTree\bin\Debug\FingerTree.dll"
open CmdQ.FingerTree

let test = seq {1..200000} |> ConcatDeque.ofSeq
let back = test |> ConcatDeque.toList

let test = RandomAccess.ofList []
test |> RandomAccess.toList |> printfn "%A"
test |> RandomAccess.tryItem 0
test |> RandomAccess.tryItem 5
test |> RandomAccess.tryItem 9
test |> RandomAccess.tryItem 10
test |> RandomAccess.tryItem 100
