module Solver
import Dijkstra
import Grid
import Data.Fin

export
solve : {n: Nat} -> {m: Nat} -> (s: Nat) -> Grid (S n) (S m) Integer -> Maybe Integer
solve s g =
  let (start, end) = (MkV2 0 0 0 0, MkV2 last last last last) in
  dijkstra cost start end
  where
    cost : V2 (S s) (S s) (S n) (S m) -> List (V2 (S s) (S s) (S n) (S m), Integer)
    cost p = map (\p' => (p', gridAt g p')) (neighbours p)
