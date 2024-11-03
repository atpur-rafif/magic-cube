module Compute(compute) where

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

compute :: (State s, Genetic s) => (Value -> IO ()) -> ComputeRequest -> s -> IO s
compute a cr s =
  let na :: (ToJSON v) => (v -> IO ())
      na m = let v = toJSON m in a v

      -- ra: runnable action
      -- p: parameter
      -- ist: initial state
      runner ist ra = case algorithm cr of
        HillClimb p -> HC.run ra p ist
        HillClimbRandomRestart p -> HCR.run ra p ist
        HillClimbStochastic p -> HCS.run ra p ist
        HillClimbWithSideway p -> HCWS.run ra p ist
        GeneticAlgorithm p -> GA.run ra p ist
        SimulatedAnnealing p -> SA.run ra p ist
   in runner s na
