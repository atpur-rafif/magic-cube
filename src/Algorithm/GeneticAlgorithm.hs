{-# Language ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Algorithm.GeneticAlgorithm where

import Algorithm (Algorithm, iterateIO)
import Control.Monad (forM)
import qualified Control.Monad.Random as R
import CubeState (GeneticAlgorithmAI (combineGenes), StateAI (generateRandomState, getPoint))
import System.Random (randomIO)

data Parameter = Parameter
  { maxIteration :: Int,
    populationSize :: Int
  }

-- TODO: Change replicate to random state
run :: forall s. (StateAI s, GeneticAlgorithmAI s) => Algorithm Parameter () s
run a p s = g <$> iterateIO (maxIteration p, replicate (populationSize p) s) f
  where
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
          then generateRandomState c
          else return c
      return $ Just (i - 1, nss)

    g :: (Int, [s]) -> s
    g = foldr1 h . snd
      where
        h c r = if getPoint c > getPoint r then c else r
