module Main where

import DataParsing.VrpTxtParse --add to force compilation
import Behaviour.NodeAndPathCalculator
import Behaviour.Genetics.Algorithm
import Data.String.Utils
import Domain
import Parameters
main :: IO()
main = do
  print("------------- VRP Genetics---------------------")
  print("Parse input File")
  fileContent <- readSingleFile $ head $ getInstanceFiles $ 0
  pressKeyToContinue
  print("initial population")
  pop <- unwrapRVar $ generateRandomPaths populationNumber [] (nodes fileContent) (vehiclesCapacity fileContent)
  prettyPrintPathList pop
  pressKeyToContinue
  print("montecarlo Selection")
  m <- montecarlo pop populationNumber
  prettyPrintPathList m
  pressKeyToContinue
  print("crossover: random esctraction and parent selection")
  c <- selectForCrossOver m
  prettyPrintPathPairs c
  pressKeyToContinue
  print("crossover: child Generation, new population")
  childs <- mapM (\x -> crossoverTwoPath x) c
  prettyPrintPathList $ filter (\x -> validator (vehiclesCapacity fileContent) x) $ m ++ ( flattenPathPairList ( childs ))
  pressKeyToContinue
  print("Apply Mutation")
  mutatedPop <- applyMutation $ m ++ ( flattenPathPairList ( childs ))
  prettyPrintPathList mutatedPop
  pressKeyToContinue
  print("Best Path of this iteration")
  print $ selectBestPath (tail mutatedPop) (head mutatedPop)
  pressKeyToContinue
  

pressKeyToContinue :: IO ()
pressKeyToContinue =
  do
    print("press key to continue")
    getChar
    return ()

prettyPrintPathList :: (Show a) => [a] -> IO ()
prettyPrintPathList paths =
  let
    input = show paths
  in
   putStr (
    replace "[[" "[\n  [" (
        replace "]]" "]\n  ]" (
            replace "],[" "],\n  [" input )))

prettyPrintPathPairs :: (Show a) => [(a,a)] -> IO ()
prettyPrintPathPairs paths =
  let
    input = show paths
  in
   putStr (
    replace "[([" "[\n  (\n    [" (
        replace "])]" "\n    ]\n  )\n]" (
            replace "],[" "],\n    [" (
                replace "]),([" "]\n  ),\n  (\n    [" input ))))
