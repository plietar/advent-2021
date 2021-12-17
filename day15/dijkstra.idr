module Dijkstra

import Data.Maybe
import BinomialTree
import Data.List
import Data.SortedSet
import Data.SortedMap
import IntMap

minWith : Ord b => (a -> b) -> a -> a -> a
minWith f x y = if f x < f y then x else y

minimumWith : Ord b => (a -> b) -> List a -> Maybe a
minimumWith f xs = foldl pick Nothing xs
  where
    pick : Maybe a -> a -> Maybe a
    pick (Just y) x = Just (minWith f x y)
    pick Nothing y = Just y

loop : (a -> Either a b) -> a -> b
loop f x = case f x of
                Left x' => loop f x'
                Right y => y

public export
dijkstra : Ord p => (p -> List (p, Integer)) -> p -> p -> Maybe Integer
dijkstra edges initial final = loop (uncurry step) (empty, singleton initial 0)
  where
    visit : SortedSet p -> SortedMap p Integer -> Integer -> p -> (SortedSet p, SortedMap p Integer)
    visit visited distances d node = (insert node visited, combinedDistances)
      where
        notVisited : p -> Bool
        notVisited node' = not (contains node' visited)

        newDistances : SortedMap p Integer
        newDistances = fromList (mapSnd (d+) <$> filter (notVisited . fst) (edges node))

        combinedDistances : SortedMap p Integer
        combinedDistances = mergeWith min distances newDistances

    step : SortedSet p -> SortedMap p Integer -> Either (SortedSet p, SortedMap p Integer) (Maybe Integer)
    step visited distances =
        case nextNode of
             Just (node, d) => if node == final then Right (Just d) else Left (visit visited distances d node)
             Nothing => Right Nothing
      where
        notVisited : p -> Bool
        notVisited node = not (contains node visited)

        nextNode : Maybe (p, Integer)
        nextNode = minimumWith snd (filter (notVisited . fst) (toList (distances)))

public export
dijkstra2 : Ord p => (p -> List (p, Integer)) -> p -> p -> Maybe Integer
dijkstra2 edges initial final = loop (uncurry step) (singleton (0, initial), singleton initial 0)
  where
    visit : Queue (Integer, p) -> SortedMap p Integer -> Integer -> p -> (Queue (Integer, p), SortedMap p Integer)
    visit todo distances d node = (newTodo, combinedDistances)
      where
        shouldUpdate : p -> Integer -> Bool
        shouldUpdate newNode newCost with (lookup newNode distances)
          shouldUpdate newNode newCost | Just oldCost = newCost < oldCost
          shouldUpdate newNode newCost | Nothing = True

        updates : List (p, Integer)
        updates = filter (uncurry shouldUpdate) (map (mapSnd (d+)) (edges node))

        combinedDistances : SortedMap p Integer
        combinedDistances = mergeLeft (fromList updates) distances

        newTodo : Queue (Integer, p)
        newTodo = foldr (BinomialTree.insert) todo (map (\(x,y) => (y,x)) updates)

    step : Queue (Integer, p) -> SortedMap p Integer -> Either (Queue (Integer, p), SortedMap p Integer) (Maybe Integer)
    step todo distances = case remove2 todo of
      (Just (d, node), newTodo) =>
        if node == final
        then Right (Just d)
        else if fromMaybe True ((d<) <$> lookup node distances)
             then step newTodo distances
             else Left (visit newTodo distances d node)

      (Nothing, newTodo) => Right Nothing

public export
astar : Ord p => (p -> List (p, Integer)) -> (p -> Integer) -> p -> p -> Maybe Integer
astar edges heuristic initial final = loop (uncurry step) (singleton (heuristic initial, initial, 0), singleton initial 0)
  where
    visit : Queue (Integer, p, Integer) -> SortedMap p Integer -> p -> Integer -> (Queue (Integer, p, Integer), SortedMap p Integer)
    visit todo distances node d = (newTodo, combinedDistances)
      where
        shouldUpdate : p -> Integer -> Bool
        shouldUpdate newNode newCost with (lookup newNode distances)
          shouldUpdate newNode newCost | Just oldCost = newCost < oldCost
          shouldUpdate newNode newCost | Nothing = True

        updates : List (p, Integer)
        updates = filter (uncurry shouldUpdate) (map (mapSnd (d+)) (edges node))

        combinedDistances : SortedMap p Integer
        combinedDistances = mergeLeft (fromList updates) distances

        newTodo : Queue (Integer, p, Integer)
        newTodo = foldr (BinomialTree.insert) todo (map (\(x,y) => (y + heuristic x, x, y)) updates)

    step : Queue (Integer, p, Integer) -> SortedMap p Integer -> Either (Queue (Integer, p, Integer), SortedMap p Integer) (Maybe Integer)
    step todo distances = case remove2 todo of
      (Just (_, node, d), newTodo) => if node == final then Right (Just d) else Left (visit newTodo distances node d)
      (Nothing, newTodo) => Right Nothing

public export
dijkstra3 : Ord Int => (Int -> List (Int, Integer)) -> Int -> Int -> Maybe Integer
dijkstra3 edges initial final = loop (uncurry step) (singleton (0, initial), singleton initial 0)
  where
    visit : Queue (Integer, Int) -> IntMap Integer -> Integer -> Int -> (Queue (Integer, Int), IntMap Integer)
    visit todo distances d node = (newTodo, combinedDistances)
      where
        shouldUpdate : Int -> Integer -> Bool
        shouldUpdate newNode newCost with (lookup newNode distances)
          shouldUpdate newNode newCost | Just oldCost = newCost < oldCost
          shouldUpdate newNode newCost | Nothing = True

        updates : List (Int, Integer)
        updates = filter (uncurry shouldUpdate) (map (mapSnd (d+)) (edges node))

        combinedDistances : IntMap Integer
        combinedDistances = mergeLeft (fromList updates) distances

        newTodo : Queue (Integer, Int)
        newTodo = foldr (BinomialTree.insert) todo (map (\(x,y) => (y,x)) updates)

    step : Queue (Integer, Int) -> IntMap Integer -> Either (Queue (Integer, Int), IntMap Integer) (Maybe Integer)
    step todo distances = case remove2 todo of
      (Just (d, node), newTodo) =>
        if node == final
        then Right (Just d)
        else if fromMaybe True ((d<) <$> lookup node distances)
             then step newTodo distances
             else Left (visit newTodo distances d node)

      (Nothing, newTodo) => Right Nothing
