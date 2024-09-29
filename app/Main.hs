module Main (main) where

import Lib (greet)

main :: IO ()
main = do
  putStrLn $ greet "World!"
