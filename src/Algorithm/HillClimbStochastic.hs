{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Algorithm.HillClimbStochastic where

import LocalSearch.State (State (nextRandomState, getPoint))
import Algorithm (Algorithm, iterateIO)

newtype Parameter = Parameter {
  maxIteration :: Int
}

run :: (State s) => Algorithm Parameter () s
run a p s = snd <$> iterateIO (maxIteration p, s) f
  where f (0, _) = return Nothing
        f (ci, cs) = do
          a ()
          ns <- nextRandomState cs
          return $ Just (ci - 1, if getPoint ns > getPoint cs then ns else cs)
