{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Algorithm.SimulatedAnnealing where

import Algorithm (Algorithm, iterateIO)
import Control.Monad.Random (randomRIO)
import Data.Aeson.TH (deriveJSON)
import GHC.Generics (Generic)
import LocalSearch.State (State (getPoint, nextRandomState))
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

data Data = Data
  { temperature :: Double,
    probabilityThreshold :: Double,
    stuckCount :: Int
  }
$(deriveJSON encodeOptions ''Data)

run :: (State s) => Algorithm Parameter Data s
run a p s = snd <$> iterateIO (sd, s) f
  where
    sd =
      Data
        { temperature = initialTemperature p,
          probabilityThreshold = 0,
          stuckCount = 0
        }
    f (Data 0 _ _, _) = return Nothing
    f (d, cs) = do
      ns <- nextRandomState cs
      let de = getPoint ns - getPoint cs
          ct = temperature d
          pv = exp $ (fromIntegral de :: Double) / ct
          nt = g ct
          nd = d {probabilityThreshold = pv, temperature = nt}
      a (cs, nd)
      if de > 0
        then return $ Just (nd, ns)
        else do
          r <- randomRIO (0, 1) :: IO Double
          let nnd = nd {stuckCount = stuckCount nd + 1}
          if r < pv
            then return $ Just (nnd, ns)
            else return $ Just (nnd, cs)

    g :: Double -> Double
    g t = case function p of
      Exponential d -> if t < 1e-10 then 0 else t / d
      Linear m -> t - m
