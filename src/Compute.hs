{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Compute (AlgorithmType (..), ComputeRequest, Request) where

import Data.Aeson as JN
import Data.Aeson.Types (Parser)
import GHC.Generics (Generic)

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
