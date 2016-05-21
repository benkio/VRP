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
  print("all pairs")
  prettyPrintPathPairs (pathPair m)
  pressKeyToContinue
  print("crossover")
  c <- selectForCrossOver m
  prettyPrintPathPairs c
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
