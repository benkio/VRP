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

-- Unwrap the Monad and print return it's content as IO Monad
unwrapRVar :: RVar a -> IO a
unwrapRVar a = do
                b <- runRVar a StdRandom
                return b

-- unwrapRVar and print It
printRVar :: (Show a) => RVar a -> IO ()
printRVar a =
  do
    b <- unwrapRVar a
    print b

-- Return a random from 0.0 to Max (float)
rand :: (Random a) => a -> a -> IO a
rand x y = newStdGen >>= return . fst . randomR (x,y)

-- Generate a Random List of length z of values between x and y
randList :: (Random a) => a -> a -> Int -> IO [a]
randList x y z = sequence $ map (\_ -> rand x y) [1..z]

-- return the shortes list length
getShorterLength :: [a] -> [a] -> Int
getShorterLength xs ys = if (length xs < length ys)
                         then length xs
                         else length ys

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

-- If a path is invalid it remove the last node until it's valid
restoreInvalidPath :: Int -> Path -> Path
restoreInvalidPath vc xs = if (validator vc xs)
                           then xs
                           else restoreInvalidPath vc z
                                 where
                                   z = if (duplicateCheck xs) then nub xs else init xs

{--------------------------------------------------------------------------------------

                         MONTECARLO EXTRACTION FUNCITONS

 ---------------------------------------------------------------------------------------}

{-
     Do a montecarlo selection on the input population and return a new population
-}
singleMontecarloExtraction :: [Path] -> IO Path
singleMontecarloExtraction paths =
  do
    r <- rand 0.0 $ totalFitness paths
    return $ montecarloPick paths r


-- Function that do the montecarlo, uses the previous function and concat single montecarlo extraction
montecarlo :: [Path] -> Int -> IO [Path]
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
{-
    From a list of paths and the random of the montecarlo estraction
    Check in what range it is and return the element of the population
-}
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

{-
    From a path and a list of path, return a new list with all the pairs between the
    starting path and the list. If the element is in the list the (x,x) pair is skipped
-}
pathPairBuilder :: Path -> [Path] -> [(Path,Path)]
pathPairBuilder _ [] = []
pathPairBuilder x (z:zs) = if (x == z)
                           then pathPairBuilder x zs
                           else (x,z) : pathPairBuilder x zs

{-
     From a list of path return a list of all the pairs between all the element in the list.
-}
pathPair :: [Path] -> [(Path,Path)]
pathPair xs = concatMap (\y -> pathPairBuilder y xs) xs

{-
    From a list of floats(randoms) and a list of pairs of paths
    return a list of pairs of paths filtered by the crossover probability
-}
selectPairByFloat :: [Float] -> [(Path,Path)] -> [(Path,Path)] -> [(Path,Path)]
selectPairByFloat _ [] acc = acc
selectPairByFloat [] _ acc = acc
selectPairByFloat (x:xs) (y:ys) acc = if (x >= crossoverProbability)
                                      then selectPairByFloat xs ys (y : acc)
                                      else selectPairByFloat xs ys acc

{-
    From a list of paths it return the crossover selection. see previous functions
-}
selectForCrossOver :: [Path] -> IO [(Path, Path)]
selectForCrossOver [] = return []
selectForCrossOver xs =
  let
    pairs = pathPair xs
  in
    do
      r <- randList 0.0 1.0 (length pairs)
      print r
      return (selectPairByFloat r (pairs) [])

{-
    Generate 2 random values between the list length in input
-}
generateTwoPointCrossoverIndices :: Int -> IO (Int,Int)
generateTwoPointCrossoverIndices listLength =
  do
    r1 <- rand 0 listLength
    r2 <- rand r1 listLength
    if (r1 /= r2) then return (r1,r2) else generateTwoPointCrossoverIndices listLength

{-
    From a path and 2 input nodes this return a new path
    with the input nodes swapped.
-}
swapNodes :: Path -> Node -> Node -> Path
swapNodes [] _ _ = []
swapNodes (x:xs) n m
  | n == x = m : (swapNodes xs n m)
  | m == x = n : (swapNodes xs n m)
  | otherwise = x : (swapNodes xs n m)

{-
    Estract a portion of a list form the input one.
-}
getSubList :: [a] -> Int -> Int -> [a]
getSubList xs a b = take (b-a) $ drop a xs

{-
    Inject a sublist in the first argument
    starting from the third and forth argument indices
-}
injectSubList :: [a] -> [a] -> Int -> Int -> [a]
injectSubList xs ys a b = (take a xs) ++ (getSubList ys a b) ++ (drop b xs)

{-
    From the pair of paths in input this swap the random inner part
    and return the result if the path is valid
-}
crossoverTwoPath :: (Path, Path) -> Int -> IO (Path, Path)
crossoverTwoPath (x,y) vc =
  let
    f xs = restoreInvalidPath vc xs
  in
    do
      (r1,r2) <- generateTwoPointCrossoverIndices (getShorterLength x y)
      print r1
      print r2
      return (f (injectSubList x y r1 r2),f (injectSubList y x r1 r2))

