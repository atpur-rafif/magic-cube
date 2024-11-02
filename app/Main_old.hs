{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main_old (main) where

import CubeState (CubeState, MatrixCube, StateAI (getPoint), cubeFromState, stateFromCube)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

main :: IO ()
main = do
  putStrLn "Hello, world!"

benchmark :: IO a -> IO (Integer, a)
benchmark a = do
  s <- getCPUTime
  r <- a
  e <- getCPUTime
  return (e - s, r)

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
solutionCubeState = stateFromCube 5 undefined solutionCube
