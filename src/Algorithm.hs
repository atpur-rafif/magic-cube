module Algorithm
  ( shuffleState,
    iterateIO,
    pickRandom,
    IterationIO,
    Algorithm,
  )
where

import CubeState (StateAI (generateRandomState))
import Data.Aeson.Types (Pair)
import System.Random (randomIO)

-- Type: State -> Parameter -> Data
-- Function: Parameter -> State -> Caller IO -> State
type Algorithm p d s = (d -> IO ()) -> p -> s -> IO s

iterateIO :: s -> (s -> IO (Maybe s)) -> IO s
iterateIO s f = do
  r <- f s
  case r of
    Just s' -> iterateIO s' f
    Nothing -> return s

pickRandom :: [a] -> IO a
pickRandom xs = do
  n <- randomIO :: IO Int
  let l = length xs
      i = n `mod` l
  return $ xs !! i

shuffleState :: (StateAI s) => Int -> s -> IO s
shuffleState 0 s = return s
shuffleState i s = do
  ns <- generateRandomState s
  shuffleState (i - 1) ns

type IterationIO = [Pair] -> IO ()
