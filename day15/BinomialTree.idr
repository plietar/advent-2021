module BinomialTree

import Data.List1

mutual
  data Node : Nat -> Type -> Type where
    MkNode : a -> (Children n a) -> Node n a

  data Children : Nat -> Type -> Type where
    Nil : Children 0 a
    (::) : Node n a -> Children n a -> Children (S n) a

link : Ord a => Node n a -> Node n a -> Node (S n) a
link (MkNode x1 cs1) (MkNode x2 cs2) =
  if x1 < x2 then MkNode x1 (MkNode x2 cs2 :: cs1)
             else MkNode x2 (MkNode x1 cs1 :: cs2)

data Queue1 : Nat -> Type -> Type where
  HQueue : Node n a -> Queue1 n a
  IQueue : Node n a -> Queue1 (S n) a -> Queue1 n a
  OQueue : Queue1 (S n) a -> Queue1 n a

export
data Queue : Type -> Type where
  Z  : Queue a
  NZ : Queue1 0 a -> Queue a

size' : Queue1 n a -> Nat
size' (HQueue _) = 1
size' (IQueue _ q) = 1 + 2 * (size' q)
size' (OQueue q) = 2 * (size' q)

size : Queue a -> Nat
size Z = 0
size (NZ q) = size' q

insertNode : Ord a => (Node n a) -> (Queue1 n a) -> (Queue1 n a)
insertNode node1 (OQueue q) = IQueue node1 q
insertNode node1 (IQueue node2 q) = OQueue (insertNode (link node1 node2) q)
insertNode node1 (HQueue node2) = OQueue (HQueue (link node1 node2))

merge : Ord a => (q1: Queue1 n a) -> (q2: Queue1 n a) -> Queue1 n a
merge (OQueue q1) (OQueue q2) = OQueue (merge q1 q2)
merge (IQueue node q1) (OQueue q2) = IQueue node (merge q1 q2)
merge (OQueue q1) (IQueue node q2) = IQueue node (merge q1 q2)
merge (HQueue node) (OQueue q2) = IQueue node q2
merge (OQueue q1) (HQueue node) = IQueue node q1
merge (HQueue node1) (HQueue node2) = OQueue (HQueue (link node1 node2))
merge (IQueue node1 q1) (IQueue node2 q2) = OQueue (insertNode (link node1 node2) (merge q1 q2))
merge (IQueue node1 q1) (HQueue node2) = OQueue (insertNode (link node1 node2) q1)
merge (HQueue node1) (IQueue node2 q2) = OQueue (insertNode (link node1 node2) q2)

empty : Queue a
empty = Z

export
singleton : a -> Queue a
singleton value = NZ (HQueue (MkNode value Nil))

export
insert : Ord a => a -> Queue a -> Queue a
insert value Z = singleton value
insert value (NZ queue) = NZ (insertNode (MkNode value Nil) queue)

roots : Queue1 n a -> List1 a
roots (HQueue (MkNode value _)) = singleton value
roots (OQueue tail) = roots tail
roots (IQueue (MkNode value _) tail) = cons value (roots tail)

minimum' : Ord a => Queue1 0 a -> a
minimum' q = foldl1 min (roots q)

minimum : Ord a => Queue a -> Maybe a
minimum Z = Nothing
minimum (NZ q) = Just (foldl1 min (roots q))

fromList : Ord a => List a -> Queue a
fromList = foldl (flip insert) empty

asHeap' : Children n a -> Queue1 n a -> Queue1 0 a
asHeap' Nil acc = acc
asHeap' (node :: tail) acc = asHeap' tail (IQueue node acc)

asHeap : Children (S n) a -> Queue1 0 a
asHeap (node :: tail) = asHeap' tail (HQueue node)

data R : Nat -> Type -> Type where
  Unchanged : R n a
  Removed : {m : Nat} -> (Maybe (Queue1 n a)) -> (Node m a) -> R n a

remove' : Ord a => {n: Nat} -> Queue1 n a -> a -> R n a
remove' (OQueue q) current =
  case remove' q current of
     Unchanged => Unchanged
     Removed {m} (Just q') node => Removed (Just (OQueue q')) node
     Removed {m} Nothing node => Removed Nothing node

remove' (IQueue n@(MkNode value children) q) current =
  case remove' q current of
    Unchanged =>
      if value < current
         then Removed (Just (OQueue q)) n
         else Unchanged
    Removed (Just q') node' => Removed (Just (IQueue n q')) node'
    Removed Nothing node' => Removed (Just (HQueue n)) node'

remove' (HQueue n@(MkNode value _)) current =
  if value < current
     then Removed Nothing n
     else Unchanged

remove'' : Ord a => {n: Nat} -> (Queue1 n a) -> (Maybe (Queue1 n a), (m ** Node m a))
remove'' (HQueue node) = (Nothing, (_ ** node))
remove'' (OQueue q) = let (q', n) = remove'' q in (OQueue <$> q', n)
remove'' (IQueue node@(MkNode value children) q) =
  case remove' q value of
       Unchanged => (Just (OQueue q), (_ ** node))
       Removed Nothing node' => (Just (HQueue node), (_ ** node'))
       Removed (Just q') node' => (Just (IQueue node q'), (_ ** node'))

remove : Ord a => (Queue1 0 a) -> (Maybe (Queue1 0 a), a)
remove q =
  case remove'' q of
    (Nothing, (0 ** MkNode value _)) => (Nothing, value)
    (Nothing, (S n ** MkNode value children)) => (Just (asHeap children), value)
    (Just q', (0 ** MkNode value _)) => (Just q', value)
    (Just q', (S n ** MkNode value children)) =>
      (Just (merge q' (asHeap children)), value)

asList' : Ord a => Queue1 0 a -> List a
asList' q =
  let (q', v) = remove q in
  v :: case q' of Just q'' => asList' q''; Nothing => []

asList : Ord a => Queue a -> List a
asList Z = []
asList (NZ q) = asList' q

export
remove2 : Ord a => Queue a -> (Maybe a, Queue a)
remove2 Z = (Nothing, Z)
remove2 (NZ q) = case remove q of
   (Nothing, value) => (Just value, Z)
   (Just q, value) => (Just value, NZ q)
