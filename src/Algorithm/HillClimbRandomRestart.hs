{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Algorithm.HillClimbRandomRestart where

import Algorithm (Algorithm, iterateIO)
import qualified Algorithm.HillClimb as HC
import Data.Aeson.TH (deriveJSON)
import GHC.Generics (Generic)
import LocalSearch.State (State (getPoint, isGlobalMaxima, randomState))
import Util (encodeOptions)

newtype Parameter = Parameter
  { maxRestart :: Int
  }
  deriving (Show, Generic)

newtype Data = Data
  { restartCount :: Int
  }

$(deriveJSON encodeOptions ''Parameter)
$(deriveJSON encodeOptions ''Data)

run :: forall s. (State s) => Algorithm Parameter Data s
run a p s = snd <$> iterateIO (maxRestart p, s) f
  where
    f (0, _) = return Nothing
    f (cr, cs) = do
      let na :: (s, ()) -> IO ()
          na (is, _) = a (is, Data {restartCount = maxRestart p - cr})
      rs <- randomState cs
      ns <- HC.run na () rs
      if isGlobalMaxima cs
        then return Nothing
        else return $ Just (cr - 1, if getPoint ns > getPoint cs then ns else cs)
