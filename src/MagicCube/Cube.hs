module MagicCube.Cube (Cube (..), Transformer (..), runTransformer, createCube, IsCube (..), matrixFromCube) where

import Line (Point (Point))
import qualified Data.Array as A

data Transformer = Digital | Analog deriving (Show)

data Cube = Cube
  { size :: Int,
    magicNumber :: Int,
    globalMaximum :: Int,
    cube :: A.Array Point Int,
    transformer :: Transformer
  } deriving (Show)

class IsCube s where
  fromCube :: Cube -> s
  toCube :: s -> Cube

  getValue :: s -> Point -> Int
  setValue :: s -> Point -> Int -> s

  switchValue :: s -> Point -> Point -> s
  switchValue s p1 p2 =
    let v1 = getValue s p1
        v2 = getValue s p2
        f (p, v) a = setValue a p v
     in foldr f s [(p1, v2), (p2, v1)]

matrixFromCube :: Cube -> [[[Int]]]
matrixFromCube c = [[[cube c A.! Point (z, y, x) | x <- [0 .. d]] | y <- [0 .. d]] | z <- [0 .. d]]
  where d = size c - 1

createCube :: [[[Int]]] -> Transformer -> Cube
createCube c t =
  Cube
    { size = s,
      magicNumber = s * (s * s * s + 1) `div` 2,
      globalMaximum = case t of
        Digital -> 3 * s * s + 6 * s + 4
        Analog -> 0,
      cube = ar,
      transformer = t
    }
  where
    m = s - 1
    ar = A.array (Point (0, 0, 0), Point (m, m, m)) $ do
      (z, l) <- zip [0 ..] c
      (y, l') <- zip [0 ..] l
      (x, e) <- zip [0 ..] l'
      return (Point (z, y, x), e)

    s =
      if d3 c
        then d
        else error "Invalid cube matrix"
      where
        d = length c

        d1 :: [a] -> Bool
        d1 = (d ==) . length

        d2 :: [[a]] -> Bool
        d2 xs = d1 xs && all d1 xs

        d3 :: [[[a]]] -> Bool
        d3 xs = d1 xs && all d2 xs


runTransformer :: Cube -> Int -> Int
runTransformer c =
  let m = magicNumber c
   in case transformer c of
        Analog -> let g i = (-1) * abs (i - m) in g
        Digital -> let g i = if i == m then 1 else 0 in g
