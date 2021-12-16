module Common

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
let readInput () = readlines () |> Seq.map Seq.toList |> Seq.map tokenize

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

let takeMiddle (l: 'a list) = l.Item(l.Length / 2)
