module Main where

import DataParsing.VrpTxtParse --add to force compilation
import Behaviour.NodeAndPathCalculator
import Behaviour.Genetics.Algorithm
import Behaviour.ACO.Solver
import Behaviour.ACO.Ant
import Data.String.Utils
import Domain
import Parameters
import GraphBuilder
import Diagrams.Backend.SVG

main :: IO()
main =
  do
      putStrLn "What Algorithm you want to run?(g - genetics, a - ants, e - exit)"
      algorithm <- getChar
      getLine
      case algorithm of
        'g' -> askInstance >>= \xs -> vrpInit xs startGenetics
        'a' -> askInstance >>= \xs -> vrpInit xs startACO
        'e' -> print ("goodbye")
        _ -> main

askInstance :: IO [Int]
askInstance =
  do
    putStrLn "What instances you want to run?(from 0 to 10 separated by space. Eg 0 or 1 2 4 etc)"
    l <- getLine
    let ls = map read $ words l :: [Int]
    return ls

vrpInit :: [Int] -> (Int -> [[Node]] -> Int -> Int -> IO()) -> IO()
vrpInit [] _ = putStrLn "!!!!!!!!!!!!!!!!!End Computation!!!!!!!!!!" >> main
vrpInit (x:xs) f = do
  print("------------- Start Computation---------------------")
  print("Parse input File #" ++ show x)
  fileContent <- readSingleFile $ head $ getInstanceFiles $ x
--  pressKeyToContinue
  let vc = vehiclesCapacity fileContent
  let n = nodes fileContent vc
  f x n vc 0 >> vrpInit xs f

startGenetics :: Int -> [[Node]] -> Int -> Int -> IO ()
startGenetics _ [] _ _ = print("------------- End Genetics---------------------")
startGenetics x (n:ns) vc i =
  let
    y = if i == 0 then show x else (show x) ++ [(['a'..'z'] !! i)]
  in
    do
      print("------------- Start Genetics---------------------")
      print (show (n:ns))
      print("initial population, Vehicle Capacity: " ++ show vc ++ " nodes: " ++ show n)
      pop <- generateRandomPaths populationNumber [] n vc
  --  prettyPrintPathList pop
  --  pressKeyToContinue
      best <- generateRandomPath n False [] vc
      genetics vc n y pop best 0 0
      startGenetics x ns vc (i+1)

genetics :: Int -> [Node] -> String -> [Domain.Path] -> Domain.Path -> Int -> Int -> IO()
genetics vc nodes' gaIstance pop best i iWithSameBest = do
  pop <- if (iWithSameBest == thrasholdUntilRandomPop)
  then (do generateRandomPaths populationNumber [] nodes' vc)
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
  let bestPath = bestPathFun mutatedPop
  print (show bestPath ++ " with fitness of: ")
  print $ calcFitness bestPath
--  pressKeyToContinue
  case ((calcFitness bestPath < calcFitness best),(i <= iterationNumber)) of
    (True,True) -> genetics vc nodes' gaIstance  mutatedPop bestPath (i+1) 0
    (False,True) -> genetics vc nodes' gaIstance mutatedPop best (i+1) (iWithSameBest+1)
    (True, False) -> ( do
                         print ("BEST PATH FOUND BY GENETIC ALGORITHM \n " ++ show bestPath ++ " with fitness of: " ++ show (calcFitness bestPath))
                         renderPretty ("bestGA"++ gaIstance ++".svg") diagramSize (pathToGraph bestPath))
    (False, False) -> ( do
                          print ("BEST PATH FOUND BY GENETIC ALGORITHM \n " ++ show best ++ " with fitness of: " ++ show (calcFitness best))
                          renderPretty ("bestGA"++ gaIstance ++".svg") diagramSize (pathToGraph best))


startACO :: Int -> [[Node]] -> Int -> Int -> IO ()
startACO _ [] _ _ = print("------------- End ACO---------------------")
startACO x (n:ns) vc i =
  let
    y = if i == 0 then show x else (show x) ++ [(['a'..'z'] !! i)]
  in
    do
      print("------------- Start ACO---------------------")
      print $ startingDataStructure n
      
pressKeyToContinue :: IO ()
pressKeyToContinue =
  do
    print("press key to continue") 
    getChar >> return ()

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
