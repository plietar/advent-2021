module Main

open Common

let rec solveB' (stack: Brace list) (input: Token list) =
    match stack, input with
    | [], [] -> None
    | stack, [] -> Some (fixScore stack)
    | _, (Open c :: rest) -> solveB' (c :: stack) rest
    | (top::stack'), (Close c :: rest) when c = top -> solveB' stack' rest
    | _, (Close c :: rest) -> None
let solveB = solveB' []

readInput ()
    |> Seq.choose solveB
    |> Seq.toList
    |> List.sort
    |> takeMiddle
    |> printfn "%d"
