#r @"../FingerTree/bin/Debug/FingerTree.dll"
open CmdQ.FingerTree

let names = "Tobias,Johannes,Erich,Doris,Dagobert,Hildegard,Erna,Michael,Elena,Henning,Thorsten".Split([| ',' |])
let pq = names |> Array.map (fun n -> n) |> RandomAccess.ofArray

RandomAccess.findIndex (fun str -> str = "Erna") pq
RandomAccess.findIndex (fun str -> str = "erna") pq
