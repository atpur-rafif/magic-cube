{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Algorithm
  ( hillClimb,
    hillClimbWithSideway,
    hillClimbStochastic,
    hillClimbRandomRestart,
    exponentialBackoff,
    simulatedAnnealing,
    geneticAlgorithm,
    shuffleState,
    IterationIO,
  )
where

import Control.Monad (forM)
import qualified Control.Monad.Random as R
import CubeState (CubeAI (isMagicCube), CubeState, GeneticAlgorithmAI (combineGenes), StateAI (generateNeighbor, generateRandomState, getPoint))
import Data.Aeson.Types (Pair, (.=))
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

type IterationIO = [Pair] -> IO ()

hillClimb :: CubeState -> IterationIO -> IO CubeState
hillClimb = hillClimbWithSideway 1

hillClimbWithSideway :: Int -> CubeState -> IterationIO -> IO CubeState
hillClimbWithSideway i s a = do
  let f 0 cs = return cs
      f ci cs = do
        ns <- pickRandom $ generateNeighbor cs
        a ["point" .= getPoint ns]
        case getPoint ns `compare` getPoint cs of
          LT -> return s
          EQ -> f (ci - 1) ns
          GT -> hillClimbWithSideway i ns a
  f i s

hillClimbStochastic :: Int -> CubeState -> IterationIO -> IO CubeState
hillClimbStochastic 0 s _ = return s
hillClimbStochastic i s a = do
  a ["point" .= getPoint s, "iteration" .= i]
  let f ns = hillClimbStochastic (i - 1) ns a
  n <- generateRandomState s
  if getPoint n > getPoint s then f n else f s

hillClimbRandomRestart :: Int -> CubeState -> IterationIO -> IO CubeState
hillClimbRandomRestart 0 s _ = return s
hillClimbRandomRestart i s a = do
  ns <- shuffleState 25 s
  hc <- hillClimb ns a
  if isMagicCube hc
    then return hc
    else do
      n <- hillClimbRandomRestart (i - 1) s a
      return $ if getPoint n > getPoint hc then n else hc

newtype TemperatureSA = TemperatureSA
  { runTemperatureSA :: (Double, TemperatureSA)
  }

exponentialBackoff :: Double -> Double -> TemperatureSA
exponentialBackoff d s
  | s < 1e-5 = TemperatureSA (s, exponentialBackoff d 0)
  | otherwise = TemperatureSA (s, exponentialBackoff d (s / d))

simulatedAnnealing :: TemperatureSA -> CubeState -> IterationIO -> IO CubeState
simulatedAnnealing t s a = do
  ns <- pickRandom $ generateNeighbor s
  let (ct, nt) = runTemperatureSA t
      d = getPoint ns - getPoint s
      nsa = simulatedAnnealing nt ns a
  a ["point" .= getPoint s, "temperature" .= ct]
  if
    | ct == 0 -> return s
    | d > 0 -> nsa
    | otherwise -> do
        let p = exp $ (fromIntegral d :: Double) / ct
        r <- randomRIO (0, 1) :: IO Double
        if r < p then nsa else return s

geneticAlgorithm :: Int -> [CubeState] -> IterationIO -> IO CubeState
geneticAlgorithm 0 ss _ = return $ foldr1 f ss
  where
    f c a = if getPoint c > getPoint a then c else a
geneticAlgorithm i ss a = do
  let ssw = if all f l then fe <$> l else l
        where
          f (_, v) = v == 0
          l = ft <$> ss
          ft s = (s, toRational $ getPoint s)
          fe (s, _) = (s, 1)
      rn = R.fromList ssw :: IO CubeState
  nss <- forM ss $ \_ -> do
    p1 <- rn
    p2 <- rn
    m <- randomIO :: IO Bool
    c <- combineGenes p1 p2
    if m
      then generateRandomState c
      else return c
  geneticAlgorithm (i - 1) nss a
