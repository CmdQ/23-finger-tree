[<AutoOpen>]
module Prelude

let inline flip f x y = f y x

module List =
    let spine list =
        let rec spine acc = function
            | [] -> invalidArg "list" "The input list was empty."
            | [last] -> List.rev acc, last
            | x::xs -> spine (x::acc) xs
        spine [] list

    let (|Snoc|_|) list =
        match list with
        | [] -> None
        | [last] -> Some([], last)
        | multiple -> multiple |> spine |> Some
