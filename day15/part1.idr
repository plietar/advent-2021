module Main

import Dijkstra
import Grid
import Solver

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
 
main : IO ()
main = do
  lines <- unfoldM tryGetLine
  let result = parseGrid lines <&> \(_ ** _ ** g) => solve 4 g
  printLn result
  pure ()
