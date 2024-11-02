module LocalSearch.State (State(..)) where

import Control.Monad.IO.Class (MonadIO)

class State s where
  getPoint :: s -> Int
  successor :: s -> [s]
  neighbor :: s -> [s]

  nextRandomState :: (MonadIO m) => s -> m s

  shuffle :: Int -> s -> IO s
  shuffle 0 s = return s
  shuffle i s = do
    ns <- nextRandomState s
    shuffle (i - 1) ns


