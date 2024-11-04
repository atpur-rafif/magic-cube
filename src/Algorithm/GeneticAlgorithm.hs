{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Algorithm.GeneticAlgorithm where

import Algorithm (Algorithm, iterateIO)
import Control.Monad (forM)
import Control.Monad.Random (replicateM)
import qualified Control.Monad.Random as R
import Data.Aeson.TH (deriveJSON)
import GHC.Generics (Generic)
import LocalSearch.Genetic (Genetic (combineGenes))
import LocalSearch.State (State (getPoint, nextRandomState, randomState))
import System.Random (randomIO)
import Util (encodeOptions)

data Parameter = Parameter
  { maxIteration :: Int,
    populationSize :: Int
  }
  deriving (Show, Generic)

newtype Data = Data
  { pointAverage :: Double
  }

$(deriveJSON encodeOptions ''Parameter)
$(deriveJSON encodeOptions ''Data)

run :: forall s. (State s, Genetic s) => Algorithm Parameter Data s
run a p s = do
  pp <- replicateM (populationSize p) $ randomState s
  let f :: (Int, [s]) -> IO (Maybe (Int, [s]))
      f (0, _) = return Nothing
      f (i, ss) = do
        let ssw = if all ((0 ==) . snd) l then fe <$> l else l
              where
                l = ft <$> ss
                ft v = (v, toRational $ getPoint v)
                fe (v, _) = (v, 1)
        let rn = R.fromList ssw
            avg = realToFrac (sum $ getPoint <$> ss) / realToFrac (populationSize p) :: Double
            ms = foldr1 g ss
              where g s1 s2 = if getPoint s1 > getPoint s2 then s1 else s2
        a (ms, Data {pointAverage = avg})
        nss <- forM ss $ \_ -> do
          p1 <- rn
          p2 <- rn
          m <- randomIO :: IO Bool
          c <- combineGenes p1 p2
          if m
            then nextRandomState c
            else return c
        return $ Just (i - 1, nss)
  ss <- iterateIO (maxIteration p, pp) f
  return $ (foldr1 h . snd) ss
  where
    h c r = if getPoint c > getPoint r then c else r
