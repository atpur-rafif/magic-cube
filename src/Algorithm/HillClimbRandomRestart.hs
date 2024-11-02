{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Algorithm.HillClimbRandomRestart where
import LocalSearch.State (State (getPoint))
import Algorithm (Algorithm, iterateIO)
import qualified Algorithm.HillClimb as HC

newtype Parameter = Parameter {
  maxRestart :: Int
}

run :: (State s) => Algorithm Parameter () s
run a p s = snd <$> iterateIO (maxRestart p, s) f
  where f (0, _) = return Nothing
        f (cr, cs) = do
          a ()
          ns <- HC.run a () cs
          return $ Just (cr - 1, if getPoint ns > getPoint cs then ns else cs)
