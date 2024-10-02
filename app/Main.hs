{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import Control.Monad (forM, forM_)
import qualified Control.Monad.Random as R
import CubeState (CubeAI (isMagicCube), CubeState, GeneticAlgorithmAI (combineGenes), MatrixCube, StateAI (generateNeighbor, generateRandomState, getPoint), stateFromCube)
import System.CPUTime (getCPUTime)
import System.Random (randomIO, randomRIO)
import Text.Printf (printf)

benchmark :: IO a -> IO (Integer, a)
benchmark a = do
  s <- getCPUTime
  r <- a
  e <- getCPUTime
  return (e - s, r)

main :: IO ()
main = do
  s <- shuffleState 20 solutionCubeState
  putStr "Original point: "
  print $ getPoint s

  let m =
        [ ("Hill Climbing", hillClimb),
          ("Hill Climbing with Sideways Move", hillClimbWithSideway 100),
          ("Sthocastic Hill Climbing", hillClimbStochastic 1000),
          ("Hill Climbing Random Restart", hillClimbRandomRestart 5),
          ("Simulated Annealing", simulatedAnnealing (exponentialBackoff 1.01 1e10)),
          ("Genetic Algorithm", geneticAlgorithm 10 . replicate 10)
        ]
  forM_ m $ \(n, a) -> do
    (t, r) <- benchmark $ a s
    printf "%s: %d (%dms)\n" n (getPoint r) (t `div` 1000000000)
    return ()

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

solutionCube :: MatrixCube
solutionCube =
  [ [ [25, 16, 80, 104, 90],
      [115, 98, 4, 1, 97],
      [42, 111, 85, 2, 75],
      [66, 72, 27, 102, 48],
      [67, 18, 119, 106, 5]
    ],
    [ [91, 77, 71, 6, 70],
      [52, 64, 117, 69, 13],
      [30, 118, 21, 123, 23],
      [26, 39, 92, 44, 114],
      [116, 17, 14, 73, 95]
    ],
    [ [47, 61, 45, 76, 86],
      [107, 43, 38, 33, 94],
      [89, 68, 63, 58, 37],
      [32, 93, 88, 83, 19],
      [40, 50, 81, 65, 79]
    ],
    [ [31, 53, 112, 109, 10],
      [12, 82, 34, 87, 100],
      [103, 3, 105, 8, 96],
      [113, 57, 9, 62, 74],
      [56, 120, 55, 49, 35]
    ],
    [ [121, 108, 7, 20, 59],
      [29, 28, 122, 125, 11],
      [51, 15, 41, 124, 84],
      [78, 54, 99, 24, 60],
      [36, 110, 46, 22, 101]
    ]
  ]

solutionCubeState :: CubeState
solutionCubeState = stateFromCube 5 solutionCube
