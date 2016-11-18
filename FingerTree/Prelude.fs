[<AutoOpen>]
module Prelude

/// Perform implicit conversion to a target type.
let inline implicit arg = (^b : (static member op_Implicit : ^a -> ^b) arg)

/// Flip the order of two arguments for a curried function.
let inline flip f x y = f y x

/// Shorthand to create a lazy from a value.
let lazyval = Lazy.CreateFromValue

/// Restrict a value to be greater than or equal to lower and less than or equal to upper.
let restrict lower value upper = min upper (max lower value)

module List =
    /// Return all but the last element of a list. This is O(n).
    let butLast list =
        let rec butLast acc = function
            | [] -> invalidArg "list" "The input list was empty."
            | [last] -> List.rev acc, last
            | x::xs -> butLast (x::acc) xs
        butLast [] list

    /// Split a list into a spine and the last element.
    let (|Snoc|_|) list =
        match list with
        | [] -> None
        | [last] -> Some([], last)
        | multiple -> multiple |> butLast |> Some
