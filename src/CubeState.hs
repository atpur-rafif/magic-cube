{-# LANGUAGE MultiParamTypeClasses #-}

module CubeState (stateFromCube, MatrixCube, CubeState, StateAI (..), CubeAI (..), GeneticAlgorithmAI (..)) where

import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO)
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import qualified GHC.Arr as A
import Line (Line (..), Point (..), generateLines, lineToPoints)
import System.Random (randomIO, randomRIO)

type MatrixCube = [[[Int]]]

data CubeState = CubeState
  { size :: Int,
    targetPoint :: Int,
    targetSum :: Int,
    currentPoint :: Int,
    cube :: A.Array Point Int,
    relatedLine :: M.Map Point [Line],
    memoizedSum :: M.Map Line Int
  }
  deriving (Show)

generateRelatedLine :: [Line] -> M.Map Point [Line]
generateRelatedLine ls = M.fromListWith (<>) t
  where
    t = mconcat $ f <$> ls
    f l = [(p, [l]) | p <- lineToPoints l]

runLine :: A.Array Point Int -> Line -> Int
runLine c l = sum $ (c A.!) <$> lineToPoints l

generateMemoizedSum :: [Line] -> A.Array Point Int -> M.Map Line Int
generateMemoizedSum ls c = M.fromList $ f <$> ls
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
    ar = A.array (Point (0, 0, 0), Point (m, m, m)) $ do
      (z, l) <- zip [0 ..] c
      (y, l') <- zip [0 ..] l
      (x, e) <- zip [0 ..] l'
      return (Point (z, y, x), e)

class StateAI s where
  getPoint :: s -> Int
  generateSuccessor :: s -> [s]
  generateNeighbor :: s -> [s]
  generateRandomState :: (MonadIO m) => s -> m s

class CubeAI s where
  getValue :: s -> Point -> Int
  setValue :: s -> Point -> Int -> s
  isMagicCube :: s -> Bool

switchValue :: (CubeAI s) => s -> Point -> Point -> s
switchValue s p1 p2 =
  let v1 = getValue s p1
      v2 = getValue s p2
      f (p, v) a = setValue a p v
   in foldr f s [(p1, v2), (p2, v1)]

instance StateAI CubeState where
  getPoint = currentPoint
  generateSuccessor s = r
    where
      m = size s - 1
      r = do
        p1@(Point (x1, y1, z1)) <- A.range (Point (0, 0, 0), Point (m, m, m))
        p2 <- A.range (Point (x1, y1, z1), Point (m, m, m))
        if p1 == p2
          then []
          else return $ switchValue s p1 p2

  generateNeighbor s = foldr f [] $ generateSuccessor s
    where
      f n [] = [n]
      f n ps@(p : _) = case getPoint n `compare` getPoint p of
        LT -> ps
        EQ -> n : ps
        GT -> [n]

  generateRandomState s = do
    let m = size s - 1
    p1 <- randomRIO (Point (0, 0, 0), Point (m, m, m))
    p2 <- randomRIO (Point (0, 0, 0), Point (m, m, m))
    if p1 == p2
      then generateRandomState s
      else return $ switchValue s p1 p2

instance CubeAI CubeState where
  getValue s p = cube s A.! p
  setValue s p v' = ns
    where
      v = cube s A.! p
      d = v' - v
      rlm = f <$> relatedLine s M.! p
        where
          f l = (l, memoizedSum s M.! l)
      nrlm = f <$> rlm
        where
          f (a, b) = (a, b + d)
      countPoint = foldr f 0
        where
          f (_, b) r = r + if b == targetSum s then 1 else 0
      ns =
        s
          { memoizedSum = M.fromList nrlm <> memoizedSum s,
            cube = cube s A.// [(p, v')],
            currentPoint = currentPoint s + countPoint nrlm - countPoint rlm
          }
  isMagicCube s = targetPoint s == currentPoint s

class GeneticAlgorithmAI s where
  combineGenes :: s -> s -> IO s

instance GeneticAlgorithmAI CubeState where
  combineGenes s1 s2 = do
    let z = zipWith f' (f s1) (f s2)
          where
            f = A.assocs . cube
            f' (p1, v1) (p2, _)
              | p1 /= p2 = error "Mismatch state"
              | otherwise = (p1, v1)
        fr f a b c = f c b a
    (p2, v2) <- fr foldM z ([], []) $ \a@(p2, v2) (p, v) -> do
      b <- randomIO :: IO Bool
      if b
        then return a
        else return (p : p2, v : v2)
    let hv2 = S.fromList v2
        s2v2 = foldl' f [] $ A.elems $ cube s2
          where
            f a v = if v `S.member` hv2 then v : a else a
        t2 = p2 `zip` s2v2
        fc (p, v) a = setValue a p v
    return $ foldr fc s1 t2
