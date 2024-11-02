module LocalSearch.Genetic (Genetic(..)) where

class Genetic s where
  combineGenes :: s -> s -> IO s


