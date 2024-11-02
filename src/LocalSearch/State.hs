module LocalSearch.State (State(..)) where

import Control.Monad.IO.Class (MonadIO)

class State s where
  getPoint :: s -> Int
  generateSuccessor :: s -> [s]
  generateNeighbor :: s -> [s]
  generateRandomState :: (MonadIO m) => s -> m s

  shuffleState :: Int -> s -> IO s
  shuffleState 0 s = return s
  shuffleState i s = do
    ns <- generateRandomState s
    shuffleState (i - 1) ns


