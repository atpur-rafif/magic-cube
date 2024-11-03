{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (encode)
import qualified Interface as I
import LocalSearch.State (State (randomState))
import MagicCube.Cube (IsCube (fromCube), Transformer (Analog, Digital), basicMatrix, createCube)
import MagicCube.MemoizedCube (MemoizedCubeState (currentCube, CubeState))
import Interface (ComputeRequest(..))
import Compute (compute)

import qualified Algorithm.HillClimbStochastic as HCS

request :: ComputeRequest
request = ComputeRequest {
  size = 10,
  algorithm = I.HillClimbStochastic $ HCS.Parameter {HCS.maxIteration = 10000000}
}

main :: IO ()
main = do
  let s = fromCube $ createCube (basicMatrix 5) Digital :: MemoizedCubeState
  r <- compute (const $ return ()) request s
  print r
  return ()
