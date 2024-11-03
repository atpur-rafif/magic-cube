{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Algorithm.GeneticAlgorithm where

import Algorithm (Algorithm, iterateIO)
import Control.Monad (forM)
import qualified Control.Monad.Random as R
import LocalSearch.Genetic (Genetic (combineGenes))
import Data.Aeson.TH (deriveJSON)
import LocalSearch.State (State (getPoint, nextRandomState, randomState))
import System.Random (randomIO)
import Control.Monad.Random (replicateM)
import GHC.Generics (Generic)
import Util (encodeOptions)

data Parameter = Parameter
  { maxIteration :: Int,
    populationSize :: Int
  } deriving (Show, Generic)

$(deriveJSON encodeOptions ''Parameter)

run :: forall s. (State s, Genetic s) => Algorithm Parameter () s
run a p s = do
  pp <- replicateM 10 $ randomState s
  let f :: (Int, [s]) -> IO (Maybe (Int, [s]))
      f (0, _) = return Nothing
      f (i, ss) = do
        a ()
        let ssw = if all ((0 ==) . snd) l then fe <$> l else l
              where
                l = ft <$> ss
                ft v = (v, toRational $ getPoint v)
                fe (v, _) = (v, 1)
            rn = R.fromList ssw
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
