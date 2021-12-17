module Dijkstra

import Data.List
import Data.SortedSet
import Data.SortedMap

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
dijkstra neighbours initial final = loop (uncurry step) (empty, singleton initial 0)
  where
    visit : SortedSet p -> SortedMap p Integer -> Integer -> p -> (SortedSet p, SortedMap p Integer)
    visit visited distances d node = (insert node visited, combinedDistances)
      where
        notVisited : p -> Bool
        notVisited node' = not (contains node' visited)

        newDistances : SortedMap p Integer
        newDistances = fromList (mapSnd (d+) <$> filter (notVisited . fst) (neighbours node))

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

