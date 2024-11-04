{-# LANGUAGE ScopedTypeVariables #-}
module Compute (compute) where

import qualified Algorithm.GeneticAlgorithm as GA
import qualified Algorithm.HillClimb as HC
import qualified Algorithm.HillClimbRandomRestart as HCR
import qualified Algorithm.HillClimbStochastic as HCS
import qualified Algorithm.HillClimbWithSideway as HCWS
import qualified Algorithm.SimulatedAnnealing as SA
import Data.Aeson (ToJSON (toJSON), Value)
import Interface (Algorithm (..), ComputeRequest (..))
import LocalSearch.Genetic (Genetic)
import LocalSearch.State (State)

compute :: forall s. ((State s, Genetic s) => ((s, Value) -> IO ()) -> ComputeRequest -> s -> IO s)
compute a cr s =
  let na :: (ToJSON v) => (s, v) -> IO ()
      na (ds, d) = let v = toJSON d in a (ds, v)
      runner = case algorithm cr of
        HillClimb p -> HC.run na p s
        HillClimbRandomRestart p -> HCR.run na p s
        HillClimbStochastic p -> HCS.run na p s
        HillClimbWithSideway p -> HCWS.run na p s
        GeneticAlgorithm p -> GA.run na p s
        SimulatedAnnealing p -> SA.run na p s
   in runner
