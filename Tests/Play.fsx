#r @"..\FingerTree\bin\Debug\FingerTree.dll"

open CmdQ

let test = RandomAccess.ofList [1..10]
test |> RandomAccess.toList |> printfn "%A"
test |> RandomAccess.tryItem 0
test |> RandomAccess.tryItem 5
test |> RandomAccess.tryItem 9
test |> RandomAccess.tryItem 10

test |> FingerTree.butLast
