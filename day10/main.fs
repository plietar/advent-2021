open System

type Brace = Curly | Round | Square | Angle
type Token = Open of Brace | Close of Brace

let rec readlines () = seq {
    let line = Console.ReadLine()
    if line <> null then
        yield line
        yield! readlines ()
}

let toToken (c: char) =
    match c with
    | '[' -> Open Square
    | ']' -> Close Square
    | '{' -> Open Curly
    | '}' -> Close Curly
    | '<' -> Open Angle
    | '>' -> Close Angle
    | '(' -> Open Round
    | ')' -> Close Round
    | _ -> failwith "Bad character"

let tokenize = List.map toToken

let errorScore t =
    match t with
    | Round -> 3UL
    | Square -> 57UL
    | Curly -> 1197UL
    | Angle -> 25137UL

let fixScore' t =
    match t with
    | Round -> 1UL
    | Square -> 2UL
    | Curly -> 3UL
    | Angle -> 4UL

let fixScore = List.fold (fun score t -> 5UL * score + fixScore' t) 0UL

let rec solveA' (stack: Brace list) (input: Token list) =
    match stack, input with
    | _, [] -> None
    | _, (Open c :: rest) -> solveA' (c :: stack) rest
    | (top::stack'), (Close c :: rest) when c = top -> solveA' stack' rest
    | _, (Close c :: rest) -> Some (errorScore c)
let solveA = solveA' []

let rec solveB' (stack: Brace list) (input: Token list) =
    match stack, input with
    | [], [] -> None
    | stack, [] -> Some (fixScore stack)
    | _, (Open c :: rest) -> solveB' (c :: stack) rest
    | (top::stack'), (Close c :: rest) when c = top -> solveB' stack' rest
    | _, (Close c :: rest) -> None
let solveB = solveB' []

let takeMiddle (l: 'a list) = l.Item(l.Length / 2)

let partA input = input |> Seq.choose solveA |> Seq.sum
let partB input =
    input
    |> Seq.choose solveB
    |> Seq.toList
    |> List.sort
    |> takeMiddle

let (|EndsWith|_|) (suffix: string) (s: string) =
    if s.EndsWith(suffix) then Some () else None

[<EntryPoint>]
let main argv =
    let input =
        readlines ()
        |> Seq.map Seq.toList
        |> Seq.map tokenize

    let result =
        match argv.[0] with
        | EndsWith("part1") -> partA input
        | EndsWith("part2") -> partB input
        | _ -> failwith "Invalid argument"

    printfn "%d" result
    0
