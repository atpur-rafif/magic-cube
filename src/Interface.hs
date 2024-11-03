{-# LANGUAGE TemplateHaskell #-}

module Interface (Request (..), Response (..), Status (..), Algorithm(..)) where

import qualified Algorithm.GeneticAlgorithm as GA
import qualified Algorithm.HillClimb as HC
import qualified Algorithm.HillClimbRandomRestart as HCR
import qualified Algorithm.HillClimbStochastic as HCS
import qualified Algorithm.HillClimbWithSideway as HCWS
import qualified Algorithm.SimulatedAnnealing as SA
import Data.Aeson.TH (deriveJSON)
import Util (encodeOptions)

data Algorithm
  = HillClimb HC.Parameter
  | HillClimbWithSideway HCWS.Parameter
  | HillClimbRandomRestart HCR.Parameter
  | HillClimbStochastic HCS.Parameter
  | SimulatedAnnealing SA.Parameter
  | GeneticAlgorithm GA.Parameter
  deriving (Show)

data RunRequest = RunRequest
  { size :: Int,
    matrix :: [[[Int]]],
    algorithm :: Algorithm
  } deriving (Show)

-- Timespan using milisecond
data Request = Request
  { force :: Maybe Bool,
    updateTimespan :: Maybe Int,
    iterationTimespan :: Maybe Int,
    run :: Maybe RunRequest
  }

data Response = Response
  { status :: Status,
    message :: String
  }

data Status = Ok | Error deriving (Show)

$(deriveJSON encodeOptions ''Algorithm)
$(deriveJSON encodeOptions ''RunRequest)
$(deriveJSON encodeOptions ''Request)

$(deriveJSON encodeOptions ''Status)
$(deriveJSON encodeOptions ''Response)
