{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Line (generateLines, lineToPoints, Point (..), Line (..)) where
import Data.Ix (Ix)

newtype Point = Point (Int, Int, Int) deriving (Eq, Ord, Show, Ix)

newtype Line = Line (Point, Point) deriving (Eq, Ord, Show)

createLine :: Point -> Point -> Line
createLine x y
  | x < y = Line (x, y)
  | otherwise = Line (y, x)

straightLine :: Int -> [Line]
straightLine n = do
  let m = n - 1
  a <- [0 .. m]
  b <- [0 .. m]
  [ Point (0, a, b) `createLine` Point (m, a, b),
    Point (a, 0, b) `createLine` Point (a, m, b),
    Point (a, b, 0) `createLine` Point (a, b, m)
    ]

planeDiagonal :: Int -> [Line]
planeDiagonal n = do
  let m = n - 1
  d <- [0 .. m]
  ((a, b), (a', b')) <-
    [ ((0, 0), (m, m)),
      ((0, m), (m, 0))
      ]
  [ Point (d, a, b) `createLine` Point (d, a', b'),
    Point (a, d, b) `createLine` Point (a', d, b'),
    Point (a, b, d) `createLine` Point (a', b', d)
    ]

spaceDiagonal :: Int -> [Line]
spaceDiagonal n =
  let m = n - 1
   in [ Point (0, 0, 0) `createLine` Point (m, m, m),
        Point (0, 0, m) `createLine` Point (m, m, 0),
        Point (m, 0, 0) `createLine` Point (0, m, m),
        Point (m, 0, m) `createLine` Point (0, m, 0)
      ]

generateLines :: Int -> [Line]
generateLines n = mconcat $ f <$> [straightLine, planeDiagonal, spaceDiagonal]
  where
    f f' = f' n

lineToPoints :: Line -> [Point]
lineToPoints (Line (Point (x1, y1, z1), Point (x2, y2, z2))) = do
  let p1 = [x1, y1, z1]
      p2 = [x2, y2, z2]
      m = foldr max 0 $ zipWith max p1 p2
  s <- [0 .. m]
  let s' = m - s
      interpolate a b = (a * s' + b * s) `div` m
      [x, y, z] = zipWith interpolate p1 p2
  return $ Point (x, y, z)
