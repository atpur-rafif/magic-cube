{-# LANGUAGE MultiParamTypeClasses #-}

module CubeState (stateFromCube, MatrixCube, CubeState, StateAI(..), CubeAI(..)) where

import Data.Map (Map, fromList, fromListWith, (!))
import GHC.Arr (Array, Ix (range), array, (!), (//))
import Line (Line (..), Point (..), generateLines, lineToPoints)

type MatrixCube = [[[Int]]]

data CubeState = CubeState
  { size :: Int,
    targetPoint :: Int,
    targetSum :: Int,
    currentPoint :: Int,
    cube :: Array Point Int,
    relatedLine :: Map Point [Line],
    memoizedSum :: Map Line Int
  }
  deriving (Show)

generateRelatedLine :: [Line] -> Map Point [Line]
generateRelatedLine ls = fromListWith (<>) t
  where
    t = mconcat $ f <$> ls
    f l = [(p, [l]) | p <- lineToPoints l]

runLine :: Array Point Int -> Line -> Int
runLine c l = sum $ (c GHC.Arr.!) <$> lineToPoints l

generateMemoizedSum :: [Line] -> Array Point Int -> Map Line Int
generateMemoizedSum ls c = fromList $ f <$> ls
  where
    f l = (l, runLine c l)

stateFromCube :: Int -> MatrixCube -> CubeState
stateFromCube s c =
  CubeState
    { size = s,
      targetPoint = tp,
      targetSum = ts,
      currentPoint = foldr f 0 ms,
      cube = ar,
      relatedLine = generateRelatedLine ls,
      memoizedSum = ms
    }
  where
    ls = generateLines s
    f v a = a + if v == ts then 1 else 0
    ts = s * (s * s * s + 1) `div` 2
    tp = length ls
    ms = generateMemoizedSum ls ar
    m = s - 1
    ar = array (Point (0, 0, 0), Point (m, m, m)) $ do
      (z, l) <- zip [0 ..] c
      (y, l') <- zip [0 ..] l
      (x, e) <- zip [0 ..] l'
      return (Point (x, y, z), e)

class StateAI s where
  getPoint :: s -> Int
  generateSuccessor :: s -> [s]

class CubeAI s where
  getValue :: s -> Point -> Int
  setValue :: s -> Point -> Int -> s
  isMagicCube :: s -> Bool

instance StateAI CubeState where
  getPoint = currentPoint
  generateSuccessor s = r
    where m = size s - 1
          r = do
            p1@(Point (x1, y1, z1)) <- range (Point (0, 0, 0), Point (m, m, m))
            p2 <- range (Point (x1, y1, z1), Point (m, m, m))
            if p1 == p2 then []
            else let 
                v1 = getValue s p1
                v2 = getValue s p2
                f (p, v) a = setValue a p v
              in return $ foldr f s [(p1, v1), (p2, v2)]


instance CubeAI CubeState where
  getValue s p = cube s GHC.Arr.! p
  setValue s p v' = ns
    where v = cube s GHC.Arr.! p
          d = v' - v
          rlm = f <$> relatedLine s Data.Map.! p
            where
              f l = (l, memoizedSum s Data.Map.! l)
          nrlm = f <$> rlm
            where
              f (a, b) = (a, b + d)
          countPoint = foldr f 0
            where
              f (_, b) r = r + if b == targetSum s then 1 else 0
          ns =
            s
              { memoizedSum = fromList nrlm <> memoizedSum s,
                cube = cube s // [(p, v')],
                currentPoint = currentPoint s + countPoint nrlm - countPoint rlm
              }
  isMagicCube s = targetPoint s == currentPoint s
