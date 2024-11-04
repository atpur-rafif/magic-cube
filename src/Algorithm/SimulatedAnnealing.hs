{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Algorithm.SimulatedAnnealing where

import Algorithm (Algorithm, iterateIO, pickRandom)
import Control.Monad.Random (randomRIO)
import Data.Aeson.TH (deriveJSON)
import GHC.Generics (Generic)
import LocalSearch.State (State (getPoint, neighbor))
import Util (encodeOptions)

data TemperatureFunction
  = Exponential
      { divisor :: Double
      }
  | Linear
      { subtractor :: Double
      }
  deriving (Show, Generic)

data Parameter = Parameter
  { initialTemperature :: Double,
    function :: TemperatureFunction
  }
  deriving (Show)

$(deriveJSON encodeOptions ''TemperatureFunction)
$(deriveJSON encodeOptions ''Parameter)

run :: (State s) => Algorithm Parameter () s
run a p s = snd <$> iterateIO (initialTemperature p, s) f
  where
    f (0, _) = return Nothing
    f (ct, cs) = do
      a (s, ())
      ns <- pickRandom $ neighbor s
      let d = getPoint ns - getPoint cs
          nt = g ct
      if d > 0
        then return $ Just (nt, ns)
        else do
          let pv = exp $ (fromIntegral d :: Double) / ct
          r <- randomRIO (0, 1) :: IO Double
          if r < pv
            then return $ Just (nt, ns)
            else return Nothing

    g :: Double -> Double
    g t = case function p of
      Exponential d -> if t < 1e-5 then 0 else d / t
      Linear m -> t - m
