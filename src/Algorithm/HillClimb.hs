{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Algorithm.HillClimb  where

import LocalSearch.State (State)
import Algorithm (Algorithm)
import qualified Algorithm.HillClimbWithSideway as HCWS

run :: (State s) => Algorithm () () s
run a _ = HCWS.run na (HCWS.Parameter 1)
  where 
        na _ = a ()
