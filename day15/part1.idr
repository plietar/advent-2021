module Main
import Data.Vect
import Data.List
import Data.String
import Data.Either
import Data.SortedSet
import Data.SortedMap
import Decidable.Equality
import Control.Monad.Trans
import Control.Monad.Error.Either
import Control.Monad.Error.Interface

data ParseError = EmptyInput | BadShape | BadRisk Char
Show ParseError where
  show EmptyInput = "bad shape"
  show BadShape = "bad shape"
  show (BadRisk _) = "bad risk"

unfoldM : Monad m => m (Maybe a) -> m (List a)
unfoldM f = do
  x <- f
  case x of
    Just y => (y::) <$> unfoldM f
    Nothing => pure []

tryGetLine : HasIO m => m (Maybe String)
tryGetLine = do
  l <- getLine
  pure (if length l == 0 then Nothing else Just l)

asVect : (n: Nat) -> List a -> Maybe (Vect n a)
asVect n l with (decEq n (length l))
  asVect n l | (Yes p) = Just (rewrite p in (fromList l))
  asVect n l | (No _) = Nothing

resize : (n: Nat) -> List a -> Either ParseError (Vect n a)
resize n l = maybeToEither BadShape (asVect n l)

readRisk : Char -> Either ParseError Int
readRisk c =
  let n = ord c in
  if n >= ord '0' && ord c <= ord '9' then pure (n - ord '0')
                                      else throwError (BadRisk c)

data Grid : Nat -> Nat -> Type -> Type where
  MkGrid : Vect n (Vect m a) -> Grid m n a

fromVect : (Vect m (Vect n a)) -> Grid n m a
fromVect = MkGrid

Functor (Grid n m) where
  map f (MkGrid g) = MkGrid (map (map f) g)

Foldable (Grid n m) where
  foldr f init (MkGrid g) = foldr (\row, acc => foldr f acc row) init g

Traversable (Grid n m) where
  traverse f (MkGrid g) = MkGrid <$> traverse (traverse f) g

readGrid : EitherT ParseError IO (n ** m ** Grid n m Int)
readGrid = do
  firstLine <- getLine <&> unpack
  rest <- unfoldM (tryGetLine <&> map unpack) >>=
    liftEither . traverse (resize (length firstLine))

  result <- traverse (liftEither . readRisk) (fromVect (fromList ((fromList firstLine) :: rest)))
  pure (_ ** _ ** result)

parseGrid : List String -> Either ParseError (n ** m ** Grid n (S m) Int)
parseGrid Nil = throwError EmptyInput
parseGrid (l::ls) = do
  let firstLine = fromList (unpack l)
  rest <- traverse (resize (length (unpack l))) (map unpack ls)
  result <- traverse readRisk (fromVect (firstLine :: (fromList rest)))
  pure (_ ** _ ** result)

bump : (n: Nat) -> Either ParseError (m ** S m = n)
bump Z = Left EmptyInput
bump (S k) = Right (k ** Refl)

parseGrid' : List String -> Either ParseError (n ** m ** Grid (S n) (S m) Int)
parseGrid' ls =
  case parseGrid ls of
   Left e => Left e
   Right (w ** h ** g) =>
    case bump w of
      Left e => Left e
      Right (w' ** p) => Right (w' ** h ** rewrite p in g)

data V2 : Nat -> Nat -> Type where
  MkV2 : Fin n -> Fin m -> V2 n m

gridAt : Grid n m a -> V2 n m ->  a
gridAt (MkGrid g) (MkV2 x y) = index x (index y g)

neighbours : {n: Nat} -> {m: Nat} -> V2 n m -> List (V2 n m)
neighbours (MkV2 x y) = catMaybes [moveX prev, moveX next, moveY prev, moveY next]
  where
    prev : Fin k -> Maybe (Fin k)
    prev FZ = Nothing
    prev (FS a) = Just (weaken a)

    next : {k: Nat} -> Fin k -> Maybe (Fin k)
    next = strengthen . FS

    moveX : ({k: Nat} -> Fin k -> Maybe (Fin k)) -> Maybe (V2 n m)
    moveX f = (\x' => MkV2 x' y) <$> f x

    moveY : ({k: Nat} -> Fin k -> Maybe (Fin k)) -> Maybe (V2 n m)
    moveY f = MkV2 x <$> f y

data Distance = Finite Int | Infinite

Show (V2 n m) where
  show (MkV2 x y) = show (x, y)

Eq (V2 n m) where
  (MkV2 x1 y1) == (MkV2 x2 y2) = (x1, y1) == (x2, y2)

Ord (V2 n m) where
  compare (MkV2 x1 y1) (MkV2 x2 y2) = compare (x1, y1) (x2, y2)

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

loop' : (a -> Either a a) -> a -> a
loop' f x = case f x of
                Left x' => x'
                Right y => y

dijkstra : Ord p => (p -> List (p, Int)) -> p -> p -> (SortedSet p, SortedMap p Int)
dijkstra neighbours initial final = loop (uncurry step) (empty, singleton initial 0)
  where
    visit : SortedSet p -> SortedMap p Int -> Int -> p -> (SortedSet p, SortedMap p Int)
    visit visited distances d node = (insert node visited, combinedDistances)
      where
        notVisited : p -> Bool
        notVisited node' = not (contains node' visited)

        newDistances : SortedMap p Int
        newDistances = fromList (mapSnd (d+) <$> filter (notVisited . fst) (neighbours node))

        combinedDistances : SortedMap p Int
        combinedDistances = mergeWith min distances newDistances

    step : SortedSet p -> SortedMap p Int -> Either (SortedSet p, SortedMap p Int) (SortedSet p, SortedMap p Int)
    step visited distances =
        case nextNode of
             Just (node, d) => if node == final then Right (visit visited distances d node) else Left (visit visited distances d node)
             Nothing => Right (visited, distances)
      where
        notVisited : p -> Bool
        notVisited node = not (contains node visited)

        nextNode : Maybe (p, Int)
        nextNode = minimumWith snd (filter (notVisited . fst) (toList (distances)))

solve : {n: Nat} -> {m: Nat} -> Grid (S n) (S m) Int -> (SortedSet (V2 (S n) (S m)), (SortedMap (V2 (S n) (S m)) Int))
solve g = dijkstra cost (MkV2 0 0) (MkV2 last last)
  where
    cost : V2 (S n) (S m) -> List (V2 (S n) (S m), Int)
    cost p = map (\p' => (p', gridAt g p')) (neighbours p)

solve' : {n: Nat} -> {m: Nat} -> Grid (S n) (S m) Int -> Maybe Int
solve' g = let (_, distances) = solve g in lookup (MkV2 last last) distances
 
main : IO ()
main = do
  lines <- unfoldM tryGetLine
  let result = parseGrid' lines <&> \(_ ** _ ** g) => solve' g
  printLn result
  pure ()
