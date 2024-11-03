{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Algorithm.HillClimbWithSideway where

import Algorithm (Algorithm, iterateIO, pickRandom)
import Data.Aeson.TH (deriveJSON)
import LocalSearch.State (State (neighbor, getPoint))
import GHC.Generics (Generic)
import Util (encodeOptions)

newtype Parameter = Parameter
  { maxIteration :: Int
  } deriving (Show, Generic)

$(deriveJSON encodeOptions ''Parameter)

run :: (State s) => Algorithm Parameter () s
run a p s = snd <$> iterateIO (maxIteration p, s) f
  where f (0, _) = return Nothing
        f (ci, cs) = do
          a ()
          ns <- pickRandom $ neighbor cs
          case getPoint ns `compare` getPoint cs of
            LT -> return Nothing
            EQ -> return $ Just (ci - 1, ns)
            GT -> return $ Just (maxIteration p, ns)
