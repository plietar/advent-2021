module PSQ

mutual
  data PSQ k p = Void | Winner (k, p) (LTree k p) k
  data LTree k p = Start | Loser (k, p) (LTree k p) k (LTree k p)

data MinView k p = Empty | Min (k, p) (PSQ k p)

play : Ord p => PSQ k p -> PSQ k p -> PSQ k p
play Void t2 = t2
play t1 Void = t1
play (Winner b t m) (Winner b' t' m') =
  if snd b <= snd b'
     then Winner b (Loser b' t m t') m'
     else Winner b' (Loser b t m t') m'

secondBest : Ord p => Ord k => LTree k p -> k -> PSQ k p
secondBest Start m = Void
secondBest (Loser b t k u) m =
  if fst b < k then play (Winner b t k) (secondBest u m)
               else play (secondBest t k) (Winner b u m)

minView : Ord p => Ord k => PSQ k p -> MinView k p
minView Void = Empty
minView (Winner b t m) = Min b (secondBest t m)

delMin : Ord k => Ord p => PSQ k p -> PSQ k p
delMin Void = Void
delMin (Winner b Start m) = Void
delMin (Winner b (Loser b' t k u) m) =
  if fst b' <= k then play (Winner b' t k) (delMin (Winner b u m))
                 else play (delMin (Winner b t k )) (Winner b' u m)

data TournamentView k p = Null | Single (k, p) | Play (PSQ k p) (PSQ k p)
tournamentView : Ord k => PSQ k p -> TournamentView k p
tournamentView Void = Null
tournamentView (Winner b Start m) = Single b
tournamentView (Winner b (Loser b' t k u) m) =
  if fst b' <= k then Play (Winner b' t k) (Winner b u m)
                 else Play (Winner b t k ) (Winner b' u m)

maxKey : PSQ k p -> Maybe k
maxKey (Winner b t m) = Just m
maxKey Void = Nothing

lookup : Ord k => k -> PSQ k p -> Maybe p
lookup k pq with (tournamentView pq)
 lookup k pq | Null  = Nothing
 lookup k pq | Single (k', prio) = if k == k' then Just prio else Nothing
 lookup k pq | Play t1 t2 =
   case maxKey t1 of Just maxT1 => if k <= maxT1
                                      then lookup k t1
                                      else lookup k t2
                     Nothing => Nothing

empty : PSQ k p
empty = Void

singleton : (k, p) -> PSQ k p
singleton b@(key, _) = Winner b Start key

adjust :: (p -> p) -> k -> PSQ k p -> PSQ k p
adjust f k ∅ = ∅
adjust f k {b}
| k key b = {k , f (prio b)}
| otherwise = {b}
adjust f k (tl & tr )
| k 6 max-key tl = adjust f k tl & tr
| otherwise = tl & adjust f k tr
