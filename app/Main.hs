module Main (main) where

import MagicCube.Cube (basicMatrix, createCube, Transformer (Analog), IsCube (fromCube))
import LocalSearch.State (State(randomState))
import MagicCube.MemoizedCube (CubeState (currentCube))

main :: IO ()
main = do
  let c = fromCube $ createCube (basicMatrix 2) Analog :: CubeState
  print $ currentCube c
  n <- randomState c
  print $ currentCube n
  return ()
