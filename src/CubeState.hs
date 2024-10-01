{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CubeState (stateFromCube, MonadCube (..), runCubeT, CubeT, MatrixCube) where

import Control.Monad.State (MonadIO, MonadState (get, put), MonadTrans, StateT (runStateT))
import Data.Map (Map, fromList, fromListWith, unionWith, (!))
import GHC.Arr (Array, array, (!), (//))
import Line (Line (..), Point (..), generateLines, lineToPoints)

type MatrixCube = [[[Int]]]

data CubeState = CubeState
  { size :: Int,
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
stateFromCube s c = CubeState s ar (generateRelatedLine ls) (generateMemoizedSum ls ar)
  where
    ls = generateLines s
    ar = array (Point (0, 0, 0), Point (s, s, s)) $ do
      (z, l) <- zip [0 ..] c
      (y, l') <- zip [0 ..] l
      (x, e) <- zip [0 ..] l'
      return (Point (x, y, z), e)

class (Monad m) => MonadCube m where
  getValue :: Point -> m Int
  setValue :: Point -> Int -> m ()
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
        nm = unionWith (+) (memoizedSum s) $ fromList [(k, d) | k <- relatedLine s Data.Map.! p]
        nc = cube s // [(p, v')]
        ns = s {memoizedSum = nm, cube = nc}
    put ns
    return ()

  isMagicCube = CubeT $ do
    s <- get
    let d = size s
        c = cube s
    return $ all ((== 315) . runLine c) $ generateLines d
