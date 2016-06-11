module Main where

import DataParsing.VrpTxtParse --add to force compilation
import Behaviour.NodeAndPathCalculator
import Behaviour.Genetics.Algorithm
import Data.String.Utils
import Domain
import Parameters
import GraphBuilder
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

main :: IO()
main =
  do
      print("------------- VRP Genetics---------------------")
      print("Parse input File")
      fileContent <- readSingleFile $ head $ getInstanceFiles $ 0
      pressKeyToContinue
      let vc = vehiclesCapacity fileContent
      let n = nodes fileContent
      print("initial population")
      pop <- unwrapRVar $ generateRandomPaths populationNumber [] n vc
      prettyPrintPathList pop
      pressKeyToContinue
      best <- unwrapRVar $ generateRandomPath n False [] 0 vc
      genetics vc pop best 0

genetics :: Int -> [Domain.Path] -> Domain.Path -> Int ->  IO()
genetics v pop best i = do
  print("montecarlo Selection")
  m <- montecarlo pop populationNumber
--  prettyPrintPathList m
--  pressKeyToContinue
  print("crossover: random esctraction and parent selection")
  parent <- selectForCrossOver m
--  prettyPrintPathPairs parent
--  pressKeyToContinue
  print("crossover: child Generation, new population")
  childs <- mapM (\x -> crossoverTwoPath x) parent
--  prettyPrintPathList $ substituteParentWithChild' m parent childs v
--  prettyPrintPathList $ filter (\x -> validator (vehiclesCapacity fileContent) x) $ m ++ ( flattenPathPairList ( childs ))
--  pressKeyToContinue
  print("Apply Mutation")
  mutatedPop <- applyMutation $ substituteParentWithChild' m parent childs v
--  prettyPrintPathList mutatedPop
--  pressKeyToContinue
  print("Best Path of this iteration")
  let bestPath = selectPath (tail mutatedPop) (head mutatedPop) (\x y -> calcFitness x < calcFitness y)
  print (show bestPath ++ " with fitness of: ")
  print $ calcFitness bestPath
--  pressKeyToContinue
  case ((calcFitness bestPath < calcFitness best),(i <= iterationNumber)) of
    (True,True) -> genetics v mutatedPop bestPath (i+1)
    (False,True) -> genetics v mutatedPop best (i+1)
    (True, False) -> ( do
                         print ("BEST PATH FOUND BY GENETIC ALGORITHM \n " ++ show bestPath ++ " with fitness of: " ++ show (calcFitness bestPath))
                         mainWith  (pathToGraph bestPath))
    (False, False) -> ( do
                          print ("BEST PATH FOUND BY GENETIC ALGORITHM \n " ++ show best ++ " with fitness of: " ++ show (calcFitness best))
                          mainWith (pathToGraph best) )

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
