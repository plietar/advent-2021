module Grid

import Data.IOArray
import Data.Linear.Array
import Data.SortedMap
import Control.Monad.Error.Interface
import Data.Vect
import Decidable.Equality
import Data.List
import Data.Either

public export
data ParseError = EmptyInput | BadShape | BadRisk Char

export
Show ParseError where
  show EmptyInput = "bad shape"
  show BadShape = "bad shape"
  show (BadRisk _) = "bad risk"

asVect : (n: Nat) -> List a -> Maybe (Vect n a)
asVect n l with (decEq n (length l))
  asVect n l | (Yes p) = Just (rewrite p in (fromList l))
  asVect n l | (No _) = Nothing

resize : (n: Nat) -> List a -> Either ParseError (Vect n a)
resize n l = maybeToEither BadShape (asVect n l)

readDigit : Char -> Either ParseError Integer
readDigit c =
  let n = ord c in
  if n >= ord '0' && ord c <= ord '9'
     then pure (cast (n - ord '0'))
     else throwError (BadRisk c)

public export
data Grid : Nat -> Nat -> Type -> Type where
  MkGrid : Vect n (Vect m a) -> Grid m n a

export
Functor (Grid n m) where
  map f (MkGrid g) = MkGrid (map (map f) g)

export
Foldable (Grid n m) where
  foldr f init (MkGrid g) = foldr (\row, acc => foldr f acc row) init g

export
Traversable (Grid n m) where
  traverse f (MkGrid g) = MkGrid <$> traverse (traverse f) g

export
fromVect : (Vect m (Vect n a)) -> Grid n m a
fromVect = MkGrid

parseGrid' : List String -> Either ParseError (n ** m ** Grid n (S m) Integer)
parseGrid' Nil = throwError EmptyInput
parseGrid' (l::ls) = do
  let firstLine = fromList (unpack l)
  rest <- traverse (resize (length (unpack l))) (map unpack ls)
  result <- traverse readDigit (fromVect (firstLine :: (fromList rest)))
  pure (_ ** _ ** result)

bump : (n: Nat) -> Either ParseError (m ** S m = n)
bump Z = Left EmptyInput
bump (S k) = Right (k ** Refl)

export
parseGrid : List String -> Either ParseError (n ** m ** Grid (S n) (S m) Integer)
parseGrid ls =
  case parseGrid' ls of
   Left e => Left e
   Right (w ** h ** g) =>
    case bump w of
      Left e => Left e
      Right (w' ** p) => Right (w' ** h ** rewrite p in g)

public export
data V2 : Nat -> Nat -> Type where
  MkV2 : Fin n -> Fin m -> V2 n m

export
Show (V2 n m) where
  show (MkV2 x y) = show (x, y)

export
Eq (V2 n m) where
  (MkV2 x1 y1) == (MkV2 x2 y2) = (x1, y1) == (x2, y2)

export
Ord (V2 n m) where
  compare (MkV2 x1 y1) (MkV2 x2 y2) = compare (x1, y1) (x2, y2)

wrap : Integer -> Integer
wrap n = mod (n - 1) 9 + 1
  -- wrap ((index x (index y g)) + (finToInteger u + finToInteger v))

export
gridAt : Grid n m Integer -> V2 n m -> Integer
gridAt (MkGrid g) (MkV2 x y) = index x (index y g)

zero : Fin n -> Fin n
zero FZ = FZ
zero (FS n') = FZ

export
neighbours : {n: Nat} -> {m: Nat} -> V2 n m -> List (V2 n m)
neighbours (MkV2 x y) = catMaybes [moveX prev, moveX next, moveY prev, moveY next]
  where
    prev : {k: Nat} -> Fin k -> Maybe (Fin k)
    prev FZ = Nothing
    prev (FS a) = Just (weaken a)

    next : {k: Nat} -> Fin k -> Maybe (Fin k)
    next b = (strengthen (FS b))

    moveX : ({k: Nat} -> Fin k -> Maybe (Fin k)) -> Maybe (V2 n m)
    moveX f = (\x' => MkV2 x' y) <$> f x

    moveY : ({k: Nat} -> Fin k -> Maybe (Fin k)) -> Maybe (V2 n m)
    moveY f = (\y' => MkV2 x y') <$> f y

export
manhattan : V2 n m -> V2 n m -> Integer
manhattan (MkV2 x1 y1) (MkV2 x2 y2) =
  let dx = (finToInteger x1) - (finToInteger x2) in
  let dy = (finToInteger y1) - (finToInteger y2) in
  abs dx + abs dy

export
scale : {m : Nat} -> {n : Nat} -> (s: Nat) -> Grid n m Integer -> Grid (s * n) (s * m) Integer
scale s g = fromVect (concat (Fin.tabulate (\v => Fin.tabulate (\y => concat (Fin.tabulate (\u => Fin.tabulate (\x => get u v x y)))))))
 where
  get : Fin s -> Fin s -> Fin n -> Fin m -> Integer
  get u v x y = wrap (gridAt g (MkV2 x y) + (finToInteger u + finToInteger v))

scale' : {m : Nat} -> {n : Nat} -> (s: Nat) -> Grid (S n) (S m) Integer -> Grid ((S s) * (S n)) ((S s) * (S m)) Integer
scale' s = scale (S s)

export
neighbourhood : {n: Nat} -> {m: Nat} -> Grid n m Integer -> SortedMap (Integer, Integer) (List ((Integer, Integer), Integer))
neighbourhood g =
  SortedMap.fromList $ do
    x <- xs;
    y <- ys;
    let ns = neighbours (MkV2 x y);
    pure ((finToInteger x, finToInteger y), (map (\node@(MkV2 x' y') => ((finToInteger x', finToInteger y'), gridAt g node)) ns))

  where
    xs : List (Fin n)
    xs = toList (Fin.range)
    ys : List (Fin m)
    ys = toList (Fin.range)

export
neighbourhood' : {n: Nat} -> {m: Nat} -> Grid n m Integer -> SortedMap (Int, Int) (List ((Int, Int), Integer))
neighbourhood' g =
  SortedMap.fromList $ do
    x <- xs;
    y <- ys;
    let ns = neighbours (MkV2 x y);
    pure ((cast (finToInteger x), cast (finToInteger y)), (map (\node@(MkV2 x' y') => ((cast (finToInteger x'), cast (finToInteger y')), gridAt g node)) ns))

  where
    xs : List (Fin n)
    xs = toList (Fin.range)
    ys : List (Fin m)
    ys = toList (Fin.range)

export
Show a => Show (Grid n m a) where
  show (MkGrid g) = show g

export
asIOArray : HasIO io => {n: Nat} -> {m: Nat} -> Grid n m Integer -> io (IOArray (IOArray Integer))
asIOArray g = do
  result <- newArray (cast m)
  for_ range $ \y => do
    row <- newArray (cast n)
    for_ range $ \x => do
      writeArray row (cast (finToInteger x)) (gridAt g (MkV2 x y))
    writeArray result (cast (finToInteger y)) row
  pure result

public export
data Array2D a = MkArray2D Int (IArray a)

createIArray : {n: Nat} -> {m: Nat} -> ((Fin n) -> (Fin m) -> a) -> Array2D a
createIArray f = Data.Linear.Array.newArray (cast n * cast m) (add last last (cast n * cast m - 1))
  where
    add : Fin (S n) -> Fin (S m) -> Int -> (1 _ : LinArray a) -> Array2D a
    add _     FZ      _ l = toIArray l (MkArray2D (cast m))
    add FZ (FS y)     i l = add last (weaken y) i l
    add (FS x) (FS y) i l =
      let l' = write l i (f x y)
      in add (weaken x) (FS y) (i-1) l'

export
read : Array2D a -> (Int, Int) -> Maybe a
read (MkArray2D stride arr) (x, y) = read arr (x + y * stride)

export
size : Array2D a -> (Int, Int)
size (MkArray2D stride arr) = let s = size arr in (stride, div s stride)

export
last : Array2D a -> (Int, Int)
last arr = let (w, h) = size arr in (w-1, h-1)

export
asIArray : {n: Nat} -> {m: Nat} -> Grid n m Integer -> Array2D Integer
asIArray g = createIArray $ \x, y => gridAt g (MkV2 x y)

coords : V2 n m -> (Int, Int)
coords (MkV2 x y) = (cast (finToInteger x), cast (finToInteger y))

coords' : {m: Nat} -> V2 n m -> Int
coords' (MkV2 x y) = cast (finToInteger x) + cast m * cast (finToInteger y)

export
asIArray2 : {n: Nat} -> {m: Nat} -> Grid n m Integer -> Array2D (List ((Int, Int), Integer))
asIArray2 g = createIArray $ \x, y => 
    let ns : List (V2 n m)
        ns = neighbours (MkV2 x y)
    in map (\p => (coords p, gridAt g p)) ns

export
asIArray3 : {n: Nat} -> {m: Nat} -> Grid n m Integer -> Array2D (List (Int, Integer))
asIArray3 g = createIArray $ \x, y => 
    let ns : List (V2 n m)
        ns = neighbours (MkV2 x y)
    in map (\p => (coords' p, gridAt g p)) ns
