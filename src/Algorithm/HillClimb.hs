{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Algorithm.HillClimb  where

import CubeState (StateAI)
import Algorithm (Algorithm)
import qualified Algorithm.HillClimbWithSideway as HCWS

run :: (StateAI s) => Algorithm () () s
run a _ = HCWS.run na (HCWS.Parameter 1)
  where 
        na _ = a ()
