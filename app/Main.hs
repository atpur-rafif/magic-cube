{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import CubeState (CubeAI (setValue, isMagicCube), CubeState, MatrixCube, StateAI (generateNeighbor, getPoint, generateRandomState), stateFromCube)
import Line (Point (Point))
import System.Random (randomIO, randomRIO)

main :: IO ()
main = do
  print $ getPoint shufledState
  hc <- hillClimb shufledState
  print $ getPoint hc
  hcws <- hillClimbWithSideway 1 shufledState
  print $ getPoint hcws
  hcs <- hillClimbStochastic 1000000 shufledState
  print $ getPoint hcs
  sa <- simulatedAnnealing (exponentialBackoff 1e3) shufledState
  print $ getPoint sa
  return ()

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

hillClimbRandomRestart :: CubeState -> IO CubeState
hillClimbRandomRestart s = do
  hc <- hillClimb s
  if isMagicCube hc then return hc
  else hillClimbRandomRestart s

newtype TemperatureSA = TemperatureSA
  { runTemperatureSA :: (Double, TemperatureSA)
  }

exponentialBackoff :: Double -> TemperatureSA
exponentialBackoff v = TemperatureSA (v, exponentialBackoff (v / 2))

simulatedAnnealing :: TemperatureSA -> CubeState -> IO CubeState
simulatedAnnealing t s = do
  ns <- pickRandom $ generateNeighbor s
  let (ct, nt) = runTemperatureSA t
      d = getPoint ns - getPoint s
      nsa = simulatedAnnealing nt ns
  if d > 0
    then nsa
    else do
      let p = exp $ (fromIntegral d :: Double) / ct
      r <- randomRIO (0, 1) :: IO Double
      if r < p then nsa else return s

shufledState :: CubeState
shufledState =
  foldr
    (\(p, v) a -> setValue a p v)
    testCubeState
    [ (Point (0, 0, 0), 16),
      (Point (0, 0, 1), 80),
      (Point (0, 0, 2), 25),
      (Point (0, 0, 3), 90),
      (Point (0, 0, 4), 104)
    ]

testCube :: MatrixCube
testCube =
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

testCubeState :: CubeState
testCubeState = stateFromCube 5 testCube
