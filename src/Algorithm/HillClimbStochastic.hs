{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Algorithm.HillClimbStochastic where

import LocalSearch.State (State (nextRandomState, getPoint))
import Data.Aeson.TH (deriveJSON)
import Algorithm (Algorithm, iterateIO)
import GHC.Generics (Generic)
import Util (encodeOptions)

newtype Parameter = Parameter {
  maxIteration :: Int
} deriving (Show, Generic)

$(deriveJSON encodeOptions ''Parameter)

run :: (State s) => Algorithm Parameter () s
run a p s = snd <$> iterateIO (maxIteration p, s) f
  where f (0, _) = return Nothing
        f (ci, cs) = do
          a ()
          ns <- nextRandomState cs
          return $ Just (ci - 1, if getPoint ns > getPoint cs then ns else cs)
