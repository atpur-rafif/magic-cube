{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Algorithm.HillClimbWithSideway where

import Algorithm (Algorithm, iterateIO, pickRandom)
import LocalSearch.State (State (generateNeighbor, getPoint))

newtype Parameter = Parameter
  { maxIteration :: Int
  }

run :: (State s) => Algorithm Parameter () s
run a p s = snd <$> iterateIO (maxIteration p, s) f
  where f (0, _) = return Nothing
        f (ci, cs) = do
          a ()
          ns <- pickRandom $ generateNeighbor cs
          case getPoint ns `compare` getPoint cs of
            LT -> return Nothing
            EQ -> return $ Just (ci - 1, ns)
            GT -> return $ Just (maxIteration p, ns)
