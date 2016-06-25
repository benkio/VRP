module Behaviour.ACO.Ant where

import Control.Monad.State
import Domain
import Behaviour.Genetics.Algorithm
import Behaviour.ACO.Solver

initialTour :: Path
initialTour = [((0,0),0)]

{-
    Starting from:
      - the tour
      - the node
    Return a new state with the two path updated
-}
visitNode :: Path -> Node -> State () Path
visitNode t x
  | x `elem` t  = error "node already visited"
  | x `notElem` t = return (t ++ [x])
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
    ys' = filter (\((a,b),(_,_)) -> a == lastNode && not (isVisited xs b)) ys
    zs' = map (\((b,(_,_)),f) -> (b,f)) $ zip (map (\((_,b),(c,d)) -> (b,(c,d))) ys') $ moveToProbability $ map snd ys'
    ranges = montecarloRages zs' (map snd) 0
    zs'' = map (\((a,_),c) -> (a,c)) $ zip zs' ranges
