{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CubeState (stateFromCube, MonadCube (..), runCubeT, CubeT, MatrixCube) where

import Control.Monad.State (MonadIO, MonadState (get, put), MonadTrans, StateT (runStateT), gets)
import Data.Map (Map, fromList, fromListWith, (!))
import GHC.Arr (Array, array, (!), (//))
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
    ar = array (Point (0, 0, 0), Point (s, s, s)) $ do
      (z, l) <- zip [0 ..] c
      (y, l') <- zip [0 ..] l
      (x, e) <- zip [0 ..] l'
      return (Point (x, y, z), e)

class (Monad m) => MonadCube m where
  getValue :: Point -> m Int
  setValue :: Point -> Int -> m ()
  getPoint :: m Int
  isMagicCube :: m Bool

newtype CubeT m a = CubeT
  { runCubeT' :: StateT CubeState m a
  }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

runLine :: Array Point Int -> Line -> Int
runLine c l = sum $ (c GHC.Arr.!) <$> lineToPoints l

runCubeT :: CubeT m a -> CubeState -> m (a, CubeState)
runCubeT = runStateT . runCubeT'

instance (Monad m) => MonadCube (CubeT m) where
  getValue p = CubeT $ do
    s <- get
    return $ cube s GHC.Arr.! p

  setValue p v' = CubeT $ do
    s <- get
    let v = cube s GHC.Arr.! p
        d = v' - v
        rlm = f <$> relatedLine s Data.Map.! p
          where f l = (l, memoizedSum s Data.Map.! l)
        nrlm = f <$> rlm
          where f (a, b) = (a, b + d)
        countPoint = foldr f 0
          where f (_, b) r = r + if b == targetSum s then 1 else 0
        ns =
          s
            { memoizedSum = fromList nrlm <> memoizedSum s,
              cube = cube s // [(p, v')],
              currentPoint = currentPoint s + countPoint nrlm - countPoint rlm
            }
    put ns
    return ()

  getPoint = CubeT $ do
    gets currentPoint

  isMagicCube = CubeT $ do
    s <- get
    return $ targetPoint s == currentPoint s
