{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Algorithm.HillClimbRandomRestart where
import LocalSearch.State (State (getPoint))
import Algorithm (Algorithm, iterateIO)
import Data.Aeson.TH (deriveJSON)
import qualified Algorithm.HillClimb as HC
import GHC.Generics (Generic)
import Util (encodeOptions)

newtype Parameter = Parameter {
  maxRestart :: Int
} deriving (Show, Generic)

$(deriveJSON encodeOptions ''Parameter)

run :: (State s) => Algorithm Parameter () s
run a p s = snd <$> iterateIO (maxRestart p, s) f
  where f (0, _) = return Nothing
        f (cr, cs) = do
          a ()
          ns <- HC.run a () cs
          return $ Just (cr - 1, if getPoint ns > getPoint cs then ns else cs)
