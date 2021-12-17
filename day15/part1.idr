module Main
import Data.IOArray
import Data.Linear.Array

import Control.Monad.Error.Either
import Data.IOArray
import Control.Monad.Error.Interface
import Dijkstra
import Grid
import Solver
import Data.Vect
import Data.SortedMap

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
 
run : EitherT ParseError IO (Maybe Integer)
run = do
  lines <- liftIO (unfoldM tryGetLine)
  (_ ** _ ** g) <- liftEither (parseGrid lines)

  let g' = asIArray3 (scale 5 g)
  pure (solveIntMap g')

main : IO ()
main = runEitherT run >>= printLn
