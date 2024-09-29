{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Cube (MatrixCube, pointToValue, runLine) where

import Line (Line (..), Point (..), lineToPoints)

type MatrixCube = [[[Int]]]

pointToValue :: MatrixCube -> Point -> Int
pointToValue cube (Point (x, y, z)) = cube !! z !! y !! x

runLine :: MatrixCube -> Line -> Int
runLine c l = sum $ fmap (pointToValue c) (lineToPoints l)
