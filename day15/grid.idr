module Grid

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
  if n >= ord '0' && ord c <= ord '9' then pure (cast (n - ord '0'))
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
data V2 : Nat -> Nat -> Nat -> Nat -> Type where
  MkV2 : Fin s -> Fin t -> Fin n -> Fin m -> V2 s t n m

export
Show (V2 s t n m) where
  show (MkV2 u v x y) = show (u, v, x, y)

export
Eq (V2 s t n m) where
  (MkV2 u1 v1 x1 y1) == (MkV2 u2 v2 x2 y2) = (u1, v1, x1, y1) == (u2, v2, x2, y2)

export
Ord (V2 s t n m) where
  compare (MkV2 u1 v1 x1 y1) (MkV2 u2 v2 x2 y2) = compare (u1, v1, x1, y1) (u2, v2, x2, y2)
wrap : Integer -> Integer
wrap n = mod (n - 1) 9 + 1

export
gridAt : Grid n m Integer -> V2 s t n m -> Integer
gridAt (MkGrid g) (MkV2 u v x y) = wrap ((index x (index y g)) + (finToInteger u + finToInteger v))

zero : Fin n -> Fin n
zero FZ = FZ
zero (FS n') = FZ

export
neighbours : {s: Nat} -> {t: Nat} -> {n: Nat} -> {m: Nat} -> V2 s t n m -> List (V2 s t n m)
neighbours (MkV2 u v x y) = catMaybes [moveX prev, moveX next, moveY prev, moveY next]
  where
    prev : {k: Nat} -> Fin j -> Fin k -> Maybe (Fin j, Fin k)
    prev FZ FZ = Nothing
    prev (FS u) FZ = Just (weaken u, last)
    prev u (FS a) = Just (u, weaken a)

    next : {j: Nat} -> {k: Nat} -> Fin j -> Fin k -> Maybe (Fin j, Fin k)
    next a b with (strengthen (FS a), strengthen (FS b))
      next a b | (_, Just b') = Just (a, b')
      next a b | (Just a', Nothing) = Just (a', zero b)
      next a b | (Nothing, Nothing) = Nothing

    moveX : ({j: Nat} -> {k: Nat} -> Fin j -> Fin k -> Maybe (Fin j, Fin k)) -> Maybe (V2 s t n m)
    moveX f = (\(u', x') => MkV2 u' v x' y) <$> f u x

    moveY : ({j: Nat} -> {k: Nat} -> Fin j -> Fin k -> Maybe (Fin j, Fin k)) -> Maybe (V2 s t n m)
    moveY f = (\(v', y') => MkV2 u v' x y') <$> f v y

