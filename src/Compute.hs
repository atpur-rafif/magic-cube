{-# LANGUAGE OverloadedStrings #-}

module Compute (AlgorithmType (..), ComputeRequest) where

import Data.Aeson as JN

data AlgorithmType
  = HillClimbing
  | HillClimbingWithSideway
  | HillClimbingStochastic
  | HillClimbingRandomRestart
  | SimulatedAnnealing
  | GeneticAlgorithm
  deriving (Show)

instance FromJSON AlgorithmType where
  parseJSON (JN.String "HillClimbing") = return HillClimbing
  parseJSON (JN.String "HillClimbingWithSideway") = return HillClimbingWithSideway
  parseJSON (JN.String "HillClimbingStochastic") = return HillClimbingStochastic
  parseJSON (JN.String "HillClimbingRandomRestart") = return HillClimbingRandomRestart
  parseJSON (JN.String "SimulatedAnnealing") = return SimulatedAnnealing
  parseJSON (JN.String "GeneticAlgorithm") = return GeneticAlgorithm
  parseJSON _ = fail "Invalid AlgorithmType"

data ComputeRequest = ComputeRequest
  { sizeCR :: Int,
    cubeCR :: [[[Int]]],
    algorithmCR :: AlgorithmType
  }
  deriving (Show)

instance FromJSON ComputeRequest where
  parseJSON (Object o) =
    ComputeRequest
      <$> o .: "size"
      <*> o .: "cube"
      <*> o .: "algorithm"
  parseJSON _ = fail "Invalid ComputeRequest"
