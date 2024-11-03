{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (encode)
import qualified Interface as I
import LocalSearch.State (State (randomState))
import MagicCube.Cube (IsCube (fromCube), Transformer (Analog), basicMatrix, createCube)
import MagicCube.MemoizedCube (CubeState (currentCube))
import Network.WebSockets (Response (Response))

main :: IO ()
main = do
  let c = fromCube $ createCube (basicMatrix 2) Analog :: CubeState
  print $ currentCube c
  n <- randomState c
  print $ currentCube n
  return ()

test =
  encode $
    I.Response
      { I.status = I.Ok,
        I.message = "Hello"
      }

cek = encode I.Ok
