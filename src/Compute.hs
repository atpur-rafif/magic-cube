{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Compute (AlgorithmAI (..), ComputeRequest, Request (..), solve) where

import Algorithm
import CubeState (CubeState, stateFromCube, getMagicNumber, Transformer(..))
import Data.Aeson as JN
import Data.Aeson.Types (Parser)
import GHC.Generics (Generic)

data AlgorithmAI
  = HillClimbing
  | HillClimbingWithSideway
      { maximumSideway :: Int
      }
  | HillClimbingStochastic
      { iterationStochastic :: Int
      }
  | HillClimbingRandomRestart
      { restartCount :: Int
      }
  | SimulatedAnnealing
  | GeneticAlgorithm
      { poolGenetic :: Int
      }
  deriving (Show)

data TransformerAI = Digital | Analog deriving (Show, Generic)

instance FromJSON TransformerAI

instance FromJSON AlgorithmAI where
  parseJSON (Object o) = do
    n <- o .: "type" :: Parser String
    case n of
      "HillClimbing" -> return HillClimbing
      "HillClimbingWithSideway" -> HillClimbingWithSideway <$> o .: "maximum"
      "HillClimbingStochastic" -> HillClimbingStochastic <$> o .: "iteration"
      "HillClimbingRandomRestart" -> HillClimbingRandomRestart <$> o .: "count"
      "SimulatedAnnealing" -> return SimulatedAnnealing
      "GeneticAlgorithm" -> GeneticAlgorithm <$> o .: "pool"
      _ -> fail "Unknown algorithm"
  parseJSON _ = fail "Invalid algorithm"

data ComputeRequest = ComputeRequest
  { sizeCR :: Int,
    cubeCR :: [[[Int]]],
    transformer :: TransformerAI,
    algorithmCR :: AlgorithmAI
  }
  deriving (Show)

data Request = Compute ComputeRequest | Cancel
  deriving (Show)

instance FromJSON ComputeRequest where
  parseJSON (Object o) =
    ComputeRequest
      <$> o .: "size"
      <*> o .: "cube"
      <*> o .: "transformer"
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

solve :: ComputeRequest -> IterationIO -> IO CubeState
solve cr = f s
  where
    d = sizeCR cr
    m = getMagicNumber d
    t = case transformer cr of
      Analog -> let g i = (-1) * abs (i - m) in g
      Digital -> let g i = if i == m then 1 else 0 in g
    s = stateFromCube d (Transformer t) (cubeCR cr)
    f = case algorithmCR cr of
      HillClimbing -> hillClimb
      HillClimbingWithSideway a -> hillClimbWithSideway a
      HillClimbingStochastic i -> hillClimbStochastic i
      HillClimbingRandomRestart r -> hillClimbRandomRestart r
      SimulatedAnnealing -> simulatedAnnealing $ exponentialBackoff 1.1 1e10
      GeneticAlgorithm p -> geneticAlgorithm p . replicate p
