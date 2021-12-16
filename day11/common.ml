open Solver;;

let read in_chan =
  let res = Buffer.create 1024 in
  let rec loop () =
    match input_line in_chan with
    | line -> Buffer.add_string res line; Buffer.add_string res "\n"; loop ()
    | exception End_of_file -> Buffer.contents res
  in loop ()

let explode s = List.init (String.length s) (String.get s)

let rec int2nat n = if n == 0 then O else S (int2nat (n - 1))
let rec nat2int n = match n with O -> 0 | S n' -> 1 + nat2int n'
