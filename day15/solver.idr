module Solver
import Data.Linear.Array
import Dijkstra
import Grid
import Data.Fin
import Data.SortedMap

export
solve : {n: Nat} -> {m: Nat} -> Grid (S n) (S m) Integer -> Maybe Integer
solve g = astar edges heuristic (MkV2 0 0) end
  where
    end : V2 (S n) (S m)
    end = (MkV2 last last)

    heuristic : V2 (S n) (S m) -> Integer
    heuristic = manhattan end

    edges : V2 (S n) (S m) -> List (V2 (S n) (S m), Integer)
    edges p = map (\p' => (p', gridAt g p')) (neighbours p)

export
solve' : {n: Nat} -> {m: Nat} -> Grid (S n) (S m) Integer -> Maybe Integer
solve' g = astar edges heuristic (0, 0) end
  where
    end : (Integer, Integer)
    end = (natToInteger n, natToInteger m)

    ns : SortedMap (Integer, Integer) (List ((Integer, Integer), Integer))
    ns = neighbourhood g

    edges : (Integer, Integer) -> (List ((Integer, Integer), Integer))
    edges p = fromMaybe [] (lookup p ns)
  
    heuristic : (Integer, Integer) -> Integer
    heuristic (x, y) = abs (fst end - x) + abs (snd end - y)

export
solve2 : {n: Nat} -> {m: Nat} -> Grid (S n) (S m) Integer -> Maybe Integer
solve2 g = astar edges heuristic (0, 0) end
  where
    end : (Int, Int)
    end = (cast (natToInteger n), cast (natToInteger m))

    ns : SortedMap (Int, Int) (List ((Int, Int), Integer))
    ns = neighbourhood' g

    edges : (Int, Int) -> (List ((Int, Int), Integer))
    edges p = fromMaybe [] (lookup p ns)
  
    heuristic : (Int, Int) -> Integer
    heuristic (x, y) = cast (abs (fst end - x) + abs (snd end - y))

{-
export
solveIArray : Nat -> Nat -> IArray Integer -> Maybe Integer
solveIArray n m g = dijkstra edges (MkV2 0 0) end
  where
    end : V2 (S n) (S m)
    end = (MkV2 last last)

    heuristic : V2 (S n) (S m) -> Integer
    heuristic = manhattan end

    get : V2 (S n) (S m) -> Integer
    get (MkV2 x y) = fromMaybe 1000000 $
                     read g (cast (finToInteger x) + (cast (finToInteger y) * cast (1+m)))

    edges : V2 (S n) (S m) -> List (V2 (S n) (S m), Integer)
    edges p = map (\p' => (p', get p')) (neighbours p)
    -}

export
solveIArray2 : Array2D (List ((Int, Int), Integer)) -> Maybe Integer
solveIArray2 g = dijkstra2 edges (0, 0) end
  where
    end : (Int, Int)
    end = last g

    edges : (Int, Int) -> List ((Int, Int), Integer)
    edges (x, y) = fromMaybe [] $ read g (x, y)

    heuristic : (Int, Int) -> Integer
    heuristic (x, y) = cast $ abs (fst end - x) + abs (snd end - y)

export
solveIntMap : Array2D (List (Int, Integer)) -> Maybe Integer
solveIntMap (MkArray2D _ g) = dijkstra3 edges 0 (size g - 1)
  where
    edges : Int -> List (Int, Integer)
    edges p = fromMaybe [] $ read g p
