{-# LANGUAGE MultiWayIf #-}

module Algorithm
  ( hillClimb,
    hillClimbWithSideway,
    hillClimbStochastic,
    hillClimbRandomRestart,
    exponentialBackoff,
    simulatedAnnealing,
    geneticAlgorithm,
    shuffleState
  )
where

import Control.Monad (forM)
import qualified Control.Monad.Random as R
import CubeState (CubeAI (isMagicCube), CubeState, GeneticAlgorithmAI (combineGenes), StateAI (generateNeighbor, generateRandomState, getPoint))
import System.Random (randomIO, randomRIO)

shuffleState :: (StateAI s) => Int -> s -> IO s
shuffleState 0 s = return s
shuffleState i s = do
  ns <- generateRandomState s
  shuffleState (i - 1) ns

pickRandom :: [a] -> IO a
pickRandom xs = do
  n <- randomIO :: IO Int
  let l = length xs
      i = n `mod` l
  return $ xs !! i

hillClimb :: CubeState -> IO CubeState
hillClimb = hillClimbWithSideway 1

hillClimbWithSideway :: Int -> CubeState -> IO CubeState
hillClimbWithSideway i s = do
  let f 0 cs = return cs
      f ci cs = do
        ns <- pickRandom $ generateNeighbor cs
        case getPoint ns `compare` getPoint cs of
          LT -> return s
          EQ -> f (ci - 1) ns
          GT -> hillClimbWithSideway i ns
  f i s

hillClimbStochastic :: Int -> CubeState -> IO CubeState
hillClimbStochastic 0 s = return s
hillClimbStochastic i s = do
  let f = hillClimbStochastic (i - 1)
  n <- generateRandomState s
  if getPoint n > getPoint s then f n else f s

hillClimbRandomRestart :: Int -> CubeState -> IO CubeState
hillClimbRandomRestart 0 s = return s
hillClimbRandomRestart i s = do
  ns <- shuffleState 25 s
  hc <- hillClimb ns
  if isMagicCube hc
    then return hc
    else do
      n <- hillClimbRandomRestart (i - 1) s
      return $ if getPoint n > getPoint hc then n else hc

newtype TemperatureSA = TemperatureSA
  { runTemperatureSA :: (Double, TemperatureSA)
  }

exponentialBackoff :: Double -> Double -> TemperatureSA
exponentialBackoff d s
  | s < 1e-5 = TemperatureSA (s, exponentialBackoff d 0)
  | otherwise = TemperatureSA (s, exponentialBackoff d (s / d))

simulatedAnnealing :: TemperatureSA -> CubeState -> IO CubeState
simulatedAnnealing t s = do
  ns <- pickRandom $ generateNeighbor s
  let (ct, nt) = runTemperatureSA t
      d = getPoint ns - getPoint s
      nsa = simulatedAnnealing nt ns
  if
    | ct == 0 -> return s
    | d > 0 -> nsa
    | otherwise -> do
        let p = exp $ (fromIntegral d :: Double) / ct
        r <- randomRIO (0, 1) :: IO Double
        if r < p then nsa else return s

geneticAlgorithm :: Int -> [CubeState] -> IO CubeState
geneticAlgorithm 0 ss = return $ foldr1 f ss
  where
    f c a = if getPoint c > getPoint a then c else a
geneticAlgorithm i ss = do
  let ssw = if all f l then fe <$> l else l
        where
          f (_, v) = v == 0
          l = ft <$> ss
          ft s = (s, toRational $ getPoint s)
          fe (s, _) = (s, 1)
      rn = R.fromList ssw :: IO CubeState
  nss <- forM ss $ \_ -> do
    a <- rn
    b <- rn
    m <- randomIO :: IO Bool
    c <- combineGenes a b
    if m
      then generateRandomState c
      else return c
  geneticAlgorithm (i - 1) nss
