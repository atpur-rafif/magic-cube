module Main (main) where

import qualified Algorithm.HillClimbRandomRestart as HCR
import Compute (compute)
import Data.Aeson.Types (Value)
import Data.IORef
import Interface (ComputeRequest (..), Algorithm (HillClimbRandomRestart, HillClimb))
import LocalSearch.State (State (getPoint))
import MagicCube.Cube (IsCube (fromCube), Transformer (..), basicMatrix)
import MagicCube.MemoizedCube (MemoizedCubeState, createCube)
import Text.Printf (printf)
import Data.Aeson (encode)

request :: ComputeRequest
request =
  ComputeRequest
    { size = 5,
      algorithm =
        HillClimbRandomRestart $
          HCR.Parameter
            { 
              HCR.maxRestart = 10
            }
    }

hc :: ComputeRequest
hc =
  ComputeRequest
    { size = 5,
      algorithm =
        HillClimb ()
    }



state :: MemoizedCubeState
state = fromCube $ createCube (basicMatrix 5) Digital

createLogger :: (State s) => IO ((s, Value) -> IO ())
createLogger = do
  currentPoint <- newIORef (0 :: Int)
  iteration <- newIORef (0 :: Int)
  return $ \(s, d) -> do
    let cp = getPoint s
    -- p <- readIORef currentPoint
    i <- readIORef iteration
    writeIORef iteration (i + 1)
    if False
      then return ()
      else do
        printf "%d: %d\n" i cp
        print $ encode d
        writeIORef currentPoint cp

main :: IO ()
main = do
  logger <- createLogger
  _ <- compute logger request state
  return ()
