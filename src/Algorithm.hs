{-# LANGUAGE FlexibleContexts #-}

module Algorithm
  ( iterateIO,
    pickRandom,
    Algorithm
  )
where


import System.Random (randomIO)

-- Type: State -> Parameter -> Data
-- Function: Parameter -> State -> Caller IO -> State
type Algorithm p d s = ((s, d) -> IO ()) -> p -> s -> IO s

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
