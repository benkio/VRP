module Main where

import DataParsing.VrpTxtParse --add to force compilation
import Behaviour.NodeAndPathCalculator
import Behaviour.Genetics.Algorithm
import Data.String.Utils
import Domain
import Parameters
import GraphBuilder
import Diagrams.Prelude
import Diagrams.Backend.SVG

main :: IO()
main =
  do
      print("------------- VRP Genetics---------------------")
      geneticsInit vrpInstances

geneticsInit :: [Int] -> IO()
geneticsInit [] = putStrLn "!!!!!!!!!!!!!!!!!End Genetics!!!!!!!!!!"
geneticsInit (x:xs) = do
  print("Parse input File #" ++ show x)
  fileContent <- readSingleFile $ head $ getInstanceFiles $ x
--  pressKeyToContinue
  let vc = vehiclesCapacity fileContent
  let n = nodes fileContent
  print("initial population")
  pop <- unwrapRVar $ generateRandomPaths populationNumber [] n vc
--  prettyPrintPathList pop
--  pressKeyToContinue
  best <- unwrapRVar $ generateRandomPath n False [] 0 vc
  genetics vc n x pop best 0 0
  geneticsInit xs


genetics :: Int -> [Node] -> Int -> [Domain.Path] -> Domain.Path -> Int -> Int -> IO()
genetics vc nodes gaIstance pop best i iWithSameBest = do
  pop <- if (iWithSameBest == thrasholdUntilRandomPop)
  then (do unwrapRVar $ generateRandomPaths populationNumber [] nodes vc)
  else return (pop)
  print("montecarlo Selection, population length: " ++ show (length pop))
  m <- montecarlo pop populationNumber
--  prettyPrintPathList m
--  pressKeyToContinue
  print("crossover: random esctraction and parent selection, montecarlo selection length: " ++ show (length m))
  parent <- selectForCrossOver m
--  prettyPrintPathPairs parent
--  pressKeyToContinue
  print("crossover: child Generation, new population, parent length: " ++ show (length parent) )
  childs <- mapM (\x -> crossoverTwoPath x) parent
--  prettyPrintPathList $ substituteParentWithChild' m parent childs vc
--  prettyPrintPathList $ filter (\x -> validator (vehiclesCapacity fileContent) x) $ m ++ ( flattenPathPairList ( childs ))
--  pressKeyToContinue
  print("Apply Mutation, child length: " ++ show (length childs))
  mutatedPop <- applyMutation $ substituteParentWithChild' m parent childs vc
--  prettyPrintPathList mutatedPop
--  pressKeyToContinue
  print("Best Path of this iteration, length newPop: " ++ show (length mutatedPop))
  let bestPath = selectPath (tail mutatedPop) (head mutatedPop) (\x y -> calcFitness x < calcFitness y)
  print (show bestPath ++ " with fitness of: ")
  print $ calcFitness bestPath
--  pressKeyToContinue
  case ((calcFitness bestPath < calcFitness best),(i <= iterationNumber)) of
    (True,True) -> genetics vc nodes gaIstance  mutatedPop bestPath (i+1) 0
    (False,True) -> genetics vc nodes gaIstance mutatedPop best (i+1) (iWithSameBest+1)
    (True, False) -> ( do
                         print ("BEST PATH FOUND BY GENETIC ALGORITHM \n " ++ show bestPath ++ " with fitness of: " ++ show (calcFitness bestPath))
                         renderPretty ("bestGA"++ show gaIstance ++".svg") diagramSize (pathToGraph bestPath))
    (False, False) -> ( do
                          print ("BEST PATH FOUND BY GENETIC ALGORITHM \n " ++ show best ++ " with fitness of: " ++ show (calcFitness best))
                          renderPretty ("bestGA"++ show gaIstance ++".svg") diagramSize (pathToGraph best))

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
