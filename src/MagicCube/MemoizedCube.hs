module MagicCube.MemoizedCube (MemoizedCubeState (..), createCube) where

import Control.Monad (foldM)
import Control.Monad.Random (randomRIO)
import qualified Data.Array as A
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import Line (Line, Point (Point), generateLines, lineToPoints)
import LocalSearch.Genetic (Genetic (..))
import LocalSearch.State (State (..))
import MagicCube.Cube (Configuration (transformer), Cube (Cube, cube), IsCube (..), createCube, randomMatrix, runTransformer, size)
import qualified MagicCube.Cube as C
import System.Random (randomIO)

data MemoizedCubeState = CubeState
  { configuration :: Configuration,
    relatedLine :: M.Map Point [Line],
    currentPoint :: Int,
    currentCube :: A.Array Point Int,
    memoizedSum :: M.Map Line Int
  }
  deriving (Show)

instance State MemoizedCubeState where
  getPoint = currentPoint
  successor s = r
    where
      m = (size . configuration $ s) - 1
      r = do
        p1@(Point (x1, y1, z1)) <- A.range (Point (0, 0, 0), Point (m, m, m))
        p2 <- A.range (Point (x1, y1, z1), Point (m, m, m))
        if p1 == p2
          then []
          else return $ switchValue s p1 p2

  neighbor s = foldr f [] $ successor s
    where
      f n [] = [n]
      f n ps@(p : _) = case getPoint n `compare` getPoint p of
        LT -> ps
        EQ -> n : ps
        GT -> [n]

  nextRandomState s = do
    let m = (size . configuration $ s) - 1
    p1 <- randomRIO (Point (0, 0, 0), Point (m, m, m))
    p2 <- randomRIO (Point (0, 0, 0), Point (m, m, m))
    if p1 == p2
      then nextRandomState s
      else return $ switchValue s p1 p2

  randomState s = do
    let cg = configuration s
    m <- randomMatrix (size cg)
    let nc = createCube m (transformer cg)
        ncs = fromCube nc
    return ncs

instance IsCube MemoizedCubeState where
  fromCube c =
    CubeState
      { configuration = cg,
        currentPoint = sum $ runTransformer cg <$> ms,
        currentCube = cube c,
        relatedLine =
          let f l = [(p, [l]) | p <- lineToPoints l]
           in M.fromListWith (<>) (mconcat $ f <$> ls),
        memoizedSum = ms
      }
    where
      cg = C.configuration c
      d = size cg
      ls = generateLines d
      ms = M.fromList $ f <$> ls
        where
          f l = (l, sum $ (cube c A.!) <$> lineToPoints l)

  toCube s = Cube {cube = currentCube s, C.configuration = configuration s}

  getValue s p = currentCube s A.! p
  setValue s p v' = ns
    where
      v = currentCube s A.! p
      d = v' - v
      t = runTransformer . configuration $ s
      rlm = f <$> relatedLine s M.! p
        where
          f l = (l, memoizedSum s M.! l)
      nrlm = f <$> rlm
        where
          f (a, b) = (a, b + d)
      countPoint ps = sum $ t . snd <$> ps
      ns =
        s
          { memoizedSum = M.fromList nrlm <> memoizedSum s,
            currentCube = currentCube s A.// [(p, v')],
            currentPoint = currentPoint s + countPoint nrlm - countPoint rlm
          }

instance Genetic MemoizedCubeState where
  combineGenes s1 s2 = do
    let z = zipWith f' (f s1) (f s2)
          where
            f = A.assocs . currentCube
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
        s2v2 = foldl' f [] $ A.elems $ currentCube s2
          where
            f a v = if v `S.member` hv2 then v : a else a
        t2 = p2 `zip` s2v2
        fc (p, v) a = setValue a p v
    return $ foldr fc s1 t2
