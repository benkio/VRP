module Behaviour.Genetics.Algorithm where

{-
    This file contains the algorithm of the generics for the VRP
-}

import Domain
import Data.Random
import Data.Random.RVar
import System.Random
import Data.List
import Behaviour.NodeAndPathCalculator

{-------------------------------------------------------------------------------------

                                     GENERIC FUNCTIONS

--------------------------------------------------------------------------------------}


printRVar :: (Show a) => RVar a -> IO ()
printRVar a = do
                b <- runRVar a StdRandom
                print b

-- Check if the given Path is valid or not
validator :: Int -> Path -> Bool
validator veicleCapacity nodes = (pathIsValid veicleCapacity (map snd nodes)) && nodes/=[]

{-------------------------------------------------------------------------------------

                              POPULATION GENERATION FUNCTIONS

--------------------------------------------------------------------------------------}


{-
    From a list of nodes, the vehicle capacity and a generator funciton of possible paths
    Return the list of all valid path from that generator.
-}
generateValidPopulation :: [Node] -> Int -> ([Node] -> [Path]) -> [Path]
generateValidPopulation nodes veicleCapacity gen = [ b | b <- gen nodes,
                             -- b <- subsequences a,
                              validator veicleCapacity b]

-- Use the previous parts for the generation of the valid population
validPopulation :: [Node] -> Int -> [Path]
validPopulation nodes veicleCapacity = generateValidPopulation nodes veicleCapacity permutations

{-
    Generate a random path. First try a random shuffle sequence of node from the ones in input
    After a 100 recursion calls none is generated the head of the nodes will be removed.
    And all start form the beginning.
-}
generateRandomPath :: [Node] -> Bool -> Path -> Int -> Int -> RVar Path
generateRandomPath _ True finalValue _ _ = return finalValue
generateRandomPath [] _ _ _ _ = return []
generateRandomPath nodes False _ n veicleCapacity = if (n==100)
                                     then
                                       generateRandomPath (tail nodes) False [] 0 veicleCapacity
                                     else
                                       (do
                                         s <- shuffle nodes
                                         generateRandomPath nodes (validator veicleCapacity s) s (n+1) veicleCapacity)

{-
    Use the previous function to generate a fixed number of random paths
    It tries indefinitely CANNOT RETURN IF THE NUMBER OF PATH CANNOT BE GENERATED.
    the return avoid duplications and empty strings
-}
generateRandomPaths :: (Eq a, Num a) => a -> [Path] -> [Node] -> Int -> RVar [Path]
generateRandomPaths 0 acc _ _ = return acc
generateRandomPaths n acc nodes veicleCapacity = do
                            v <- generateRandomPath nodes False [] 0 veicleCapacity
                            if v `elem` acc
                            then generateRandomPaths n acc nodes veicleCapacity
                            else generateRandomPaths (n-1) (v:acc) nodes veicleCapacity
