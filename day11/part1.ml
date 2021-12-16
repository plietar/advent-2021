open Solver;;
open Common;;

let s = read stdin in
let fuel = int2nat 10000 in
let result = part1 fuel (parse (explode s)) in
print_int (nat2int result)
