module LocalSearch.State (State(..)) where

import Control.Monad.IO.Class (MonadIO)

class State s where
  getPoint :: s -> Int
  successor :: s -> [s]
  neighbor :: s -> [s]

  -- Need configuration from previous state
  randomState :: (MonadIO m) => s -> m s
  nextRandomState :: (MonadIO m) => s -> m s
  isGlobalMaxima :: s -> Bool

  shuffleState :: Int -> s -> IO s
  shuffleState 0 s = return s
  shuffleState i s = do
    ns <- nextRandomState s
    shuffleState (i - 1) ns
