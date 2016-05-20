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
import Control.Monad
import Parameters

{-------------------------------------------------------------------------------------

                                     GENERIC FUNCTIONS

--------------------------------------------------------------------------------------}


unwrapRVar :: RVar a -> IO a
unwrapRVar a = do
                b <- runRVar a StdRandom
                return b

printRVar :: (Show a) => RVar a -> IO ()
printRVar a =
  do
    b <- unwrapRVar a
    print b

-- Check if the given Path is valid or not
validator :: Int -> Path -> Bool
validator veicleCapacity nodes = (pathIsValid veicleCapacity (map snd nodes)) && nodes/=[]

-- Return a random from 0.0 to Max (float)
rand :: Float -> IO Float
rand x = newStdGen >>= return . fst . randomR (0.0,x)

randList :: Float -> Int -> IO [Float]
randList x y = sequence $ map (\_ -> rand x) [1..y]

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


{--------------------------------------------------------------------------------------

                         MONTECARLO EXTRACTION FUNCITONS

 ---------------------------------------------------------------------------------------}

{-
     Do a montecarlo selection on the input population and return a new population
-}
singleMontecarloExtraction :: [Path] -> IO Path
singleMontecarloExtraction paths =
  do
    r <- rand $ totalFitness paths
    return $ montecarloPick paths r



montecarlo :: [Path] -> Int -> IO [ Path]
montecarlo x y = sequence $ f x y
                 where
                   f _ 0 = do []
                   f paths n = singleMontecarloExtraction paths : f paths (n-1)


{-
    From the input population fitness return a list of ranges from 1 to the total fitness
-}
montecarloRages :: [Path] -> Float -> [Float]
montecarloRages [] _ = []
montecarloRages paths acc = x : montecarloRages (tail paths) x
                                where
                                  x = (acc + head (fitness paths))

montecarloPick :: [Path] -> Float -> Path
montecarloPick paths randomIndex =
  let
    ranges = montecarloRages paths 0
  in
    f randomIndex (zip paths ranges)
  where
    f :: Float -> [(Path,Float)] -> Path
    f pick ziplist =
      if ((snd (head ziplist)) > pick)
      then fst (head ziplist)
      else f pick (tail ziplist)

{--------------------------------------------------------------------------------------

                                CROSSOVER FUNCTIONS

---------------------------------------------------------------------------------------}

pathPairBuilder :: Path -> [Path] -> [(Path,Path)]
pathPairBuilder x [] = []
pathPairBuilder x (z:zs) = if (x == z)
                           then pathPairBuilder x zs
                           else (x,z) : pathPairBuilder x zs

pathPair :: [Path] -> [(Path,Path)]
pathPair xs = concatMap (\y -> pathPairBuilder y xs) xs

selectPairByFloat :: [Float] -> [(Path,Path)] -> [(Path,Path)] -> [(Path,Path)]
selectPairByFloat _ [] acc = acc
selectPairByFloat [] _ acc = acc
selectPairByFloat (x:xs) (y:ys) acc = if (x >= crossoverProbability)
                                      then selectPairByFloat xs ys (y : acc)
                                      else selectPairByFloat xs ys acc

selectForCrossOver :: [Path] -> IO [(Path, Path)]
selectForCrossOver [] = return []
selectForCrossOver xs =
  let
    pairs = pathPair xs
  in
    do
      r <- randList 1.0 (length pairs)
      print r
      return (selectPairByFloat r (pairs) [])
