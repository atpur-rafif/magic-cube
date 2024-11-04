{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Algorithm.HillClimb  where

import LocalSearch.State (State)
import Algorithm (Algorithm)
import qualified Algorithm.HillClimbWithSideway as HCWS

type Parameter = ()

run :: (State s) => Algorithm Parameter () s
run a _ = HCWS.run na (HCWS.Parameter 1)
  where 
        na (s, _) = a (s, ())
