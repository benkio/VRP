module Behaviour.ACO.Solver where

import Control.Monad.State
import Domain
import Parameters
import Behaviour.NodeAndPathCalculator
import Data.List


{-
    Setup a new datastructure for all the ACO possible moves with:
    - all the node pairs
    - the pheromone level for that pair
    - the attactivity, the less distance the higher attractivity (distance inverted between the lowers and the highers)

    The length of this structure is se summation of length of xs
-}
startingDataStructure :: [Node] -> [((Node, Node),(Float, Float))]
startingDataStructure xs = map (\(((a,b),(c,d)),e)  -> ((a,b),(c,e))) $ zip ls zs
  where
    ys = ((0,0),0) : sort xs
    ls = [((a,b),(initialPheromoneTrace, calculateDistance (fst a) (fst b)))| a <- ys, b <- ys, b>a]
    zs = inverse (map (\((_,_),(_,a)) -> a) ls) (\a -> a)

moveToProbability :: [(Float, Float)] -> [Float]
moveToProbability xs = map (\(x,y) -> (f x y)/denominator) xs
  where
    f a b = (a^beta) * (b^alfa)
    denominator = foldr (+) 0 $ map (\(x,y) -> f x y) xs

updatePheromone :: [Path] -> [((Node, Node),(Float, Float))] -> [((Node, Node),(Float, Float))]
updatePheromone xs ys = map (\(a,(c,d)) -> (a,( (c*evaporationCoefficient) + (fromIntegral (getNodePairFrequency a zs)) ,d))) ys
  where
    zs = frequency $ concat $ pairPathNodes xs
