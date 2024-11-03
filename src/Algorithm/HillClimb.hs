{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TemplateHaskell #-}
module Algorithm.HillClimb  where

import LocalSearch.State (State)
import Algorithm (Algorithm)
import qualified Algorithm.HillClimbWithSideway as HCWS
import Data.Aeson.TH (deriveJSON)
import Util (encodeOptions)

data Parameter = Parameter {} deriving (Show)
$(deriveJSON encodeOptions ''Parameter)

run :: (State s) => Algorithm Parameter () s
run a _ = HCWS.run na (HCWS.Parameter 1)
  where 
        na _ = a ()
