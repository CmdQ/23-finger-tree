[<AutoOpen>]
module Prelude

let inline implicit arg = (^b : (static member op_Implicit : ^a -> ^b) arg)

let inline flip f x y = f y x

module List =
    let butLast list =
        let rec butLast acc = function
            | [] -> invalidArg "list" "The input list was empty."
            | [last] -> List.rev acc, last
            | x::xs -> butLast (x::acc) xs
        butLast [] list

    let (|Snoc|_|) list =
        match list with
        | [] -> None
        | [last] -> Some([], last)
        | multiple -> multiple |> butLast |> Some
