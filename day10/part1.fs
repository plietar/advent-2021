module Main

open Common

let rec solveA' (stack: Brace list) (input: Token list) =
    match stack, input with
    | _, [] -> None
    | _, (Open c :: rest) -> solveA' (c :: stack) rest
    | (top::stack'), (Close c :: rest) when c = top -> solveA' stack' rest
    | _, (Close c :: rest) -> Some (errorScore c)
let solveA = solveA' []

readInput ()
    |> Seq.choose solveA
    |> Seq.sum
    |> printfn "%d"
