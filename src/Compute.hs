{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Compute (AlgorithmType (..), ComputeRequest, Request (..), solve) where

import CubeState (CubeState, stateFromCube)
import Data.Aeson as JN
import Data.Aeson.Types (Parser)
import GHC.Generics (Generic)
import Algorithm

data AlgorithmType
  = HillClimbing
  | HillClimbingWithSideway
  | HillClimbingStochastic
  | HillClimbingRandomRestart
  | SimulatedAnnealing
  | GeneticAlgorithm
  deriving (Show, Generic)

instance FromJSON AlgorithmType

data ComputeRequest = ComputeRequest
  { sizeCR :: Int,
    cubeCR :: [[[Int]]],
    algorithmCR :: AlgorithmType
  }
  deriving (Show)

data Request = Compute ComputeRequest | Cancel
  deriving (Show)

instance FromJSON ComputeRequest where
  parseJSON (Object o) =
    ComputeRequest
      <$> o .: "size"
      <*> o .: "cube"
      <*> o .: "algorithm"
  parseJSON _ = fail "Invalid ComputeRequest"

instance FromJSON Request where
  parseJSON v@(Object o) = do
    c <- o .:? "cancel" :: Parser (Maybe Bool)
    let n = Compute <$> (parseJSON v :: Parser ComputeRequest)
    case c of
      Just True -> return Cancel
      Just False -> n
      Nothing -> n
  parseJSON _ = fail "Invalid Request"

solve :: ComputeRequest -> IO CubeState
solve cr = f s
  where
    s = stateFromCube (sizeCR cr) (cubeCR cr)
    f = case algorithmCR cr of
      HillClimbing -> hillClimb
      HillClimbingWithSideway -> hillClimbWithSideway 100
      HillClimbingStochastic -> hillClimbStochastic 1000
      HillClimbingRandomRestart -> hillClimbRandomRestart 5
      SimulatedAnnealing -> simulatedAnnealing $ exponentialBackoff 1.1 1e10
      GeneticAlgorithm -> geneticAlgorithm 10 . replicate 10
