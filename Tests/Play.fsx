#r @"../FingerTree/bin/Debug/FingerTree.dll"
open CmdQ.FingerTree

let names = "Tobias,Johannes,Erich,Doris,Dagobert,Hildegard,Erna,Michael,Elena,Henning,Thorsten".Split([| ',' |])
let pq = names |> Array.map (fun n -> n, n.Length |> float32) |> PriorityQueue.ofArray

PriorityQueue.peek pq

let pq2 = PriorityQueue.add "Cheat" 99.0f pq

PriorityQueue.peek pq
PriorityQueue.peek pq2

let r = pq2 |> PriorityQueue.pop

let rec loop = function
    | PriorityQueue.Pop(h, r) ->
        printfn "%s" h
        loop r
    | _ ->
        printfn "That's it."
loop pq

pq |> PriorityQueue.toSeqWithPriorities |> Seq.toList |> printfn "%A"

PriorityQueue.peek r

let one = PriorityQueue.ofList [99, 2.0f]
PriorityQueue.peek one
let empty = one |> PriorityQueue.pop
PriorityQueue.peek empty

