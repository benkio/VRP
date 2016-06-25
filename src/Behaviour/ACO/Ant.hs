module Behaviour.ACO.Ant where

import Domain
import Behaviour.Genetics.Algorithm
import Behaviour.ACO.Solver
import Data.List

initialTour :: Path
initialTour = [((0,0),0)]

{-
    Starting from:
      - the tour
      - the node
    Return a new state with the two path updated
-}
visitNode :: Path -> Node -> Path
visitNode t x
  | x `elem` t  = error "node already visited"
  | x `notElem` t = t ++ [x]
  | otherwise = error "strange problem in a exaustive pattern"

{-
    Starting from:
      - the tour
      - the node
    Return if the node is in the path
-}
isVisited :: Path -> Node -> Bool
isVisited xs n = n `elem` xs

nextToVisit :: Path -> [((Node, Node),(Float, Float))] -> IO Node
nextToVisit xs ys = singleMontecarloExtraction 1.0 zs''
  where
    lastNode = head $ reverse xs
    ys' = filter (\((a,b),(_,_)) -> (a == lastNode && not (isVisited xs b)) || (b == lastNode && not (isVisited xs a))) ys
    zs' = map (\(((a,b),(_,_)),f) -> if a == lastNode then (b,f) else (a,f)) $ zip ys' $ moveToProbability $ map snd ys'
    ranges = montecarloRages zs' (map snd) 0
    zs'' = map (\((a,_),c) -> (a,c)) $ zip zs' ranges

buildSolution :: [((Node, Node),(Float, Float))] -> Path -> IO Path
buildSolution xs solution =
  if all (\n -> isVisited solution n) (nub (map (fst.fst) xs))
  then return solution
  else do
         next <- nextToVisit solution xs
         buildSolution xs (visitNode solution next)
