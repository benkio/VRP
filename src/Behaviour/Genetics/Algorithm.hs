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

-- Return a random from min to Max 
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

flattenPathPairList :: [(Path,Path)] -> [Path]
flattenPathPairList xs = foldr (\(a,b) ys -> ys ++ [a, b]) [] xs

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
    r <- rand 0.0 $ totalFitness paths
    case (montecarloPick paths r) of
      [] -> singleMontecarloExtraction paths
      x -> return x


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
                                  x = acc + head (fitnessInverse paths)
{-
    From a list of paths and the random of the montecarlo estraction
    Check in what range it is and return the element of the population
-}
montecarloPick :: [Path] -> Float -> Path
montecarloPick paths randomIndex =
  let
    ranges = zip paths (montecarloRages paths 0)
  in
    if (length paths == length (montecarloRages paths 0))
    then f randomIndex ranges
    else error "ERROR LENGTH"
  where
    f :: Float -> [(Path,Float)] -> Path
    f pick [] = []
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
selectPairByFloat (x:xs) (y:ys) acc = if (x <= crossoverProbability)
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
    Estract a portion of a list form the input one.
-}
getSubList :: [a] -> Int -> Int -> [a]
getSubList xs a b = take (b-a) $ drop a xs

{-
    Inject a sublist in the first argument
    starting from the third and forth argument indices
-}
injectSubList :: (Eq a) => [a] -> [a] -> Int -> Int -> [a]
injectSubList xs ys a b =
  let
    midList = (getSubList ys a b)
    zs = filter (\x -> x `notElem` midList) xs
    w = (length xs)-b
  in
  (drop w zs) ++ midList ++ (take w zs)
   

{-
    From the pair of paths in input this swap the random inner part
    and return the result if the path is valid
-}
crossoverTwoPath :: (Path, Path) -> IO (Path, Path)
crossoverTwoPath (x,y) =
    do
      (r1,r2) <- generateTwoPointCrossoverIndices (getShorterLength x y)
{-      print r1
      print r2  -}
      return (injectSubList x y r1 r2, injectSubList y x r1 r2)

{-
    Substitute a single path to che population
    1 - population
    2 - Parent
    3 - Child
-}
substitute :: [Path] -> Path -> Path -> [Path]
substitute [] _ _ = []
substitute (x:xs) p c
    | x == p = c : xs
    | otherwise = x : substitute xs p c 

{-
    Add new checks to the previous substitution, wrap it
    if the element parent is in the population and the child is valid the substitution happens
    if the parent isn't in the population the worsefitnesspath is substituted instead
    Nothing happens otherwise.
-}
substituteParentWithChild :: [Path] -> Path -> Path -> Int-> [Path]
substituteParentWithChild xs p c vc 
    | (p `elem` xs) && validator vc c = substitute xs p c
    | validator vc c = substitute xs worseFitnessPath c
    | otherwise = xs
    where
      worseFitnessPath = (selectPath (tail xs) (head xs) (\x y -> calcFitness x > calcFitness y) )

{-
    Given a population, a list of Parents and a list of Childs substitute them in a new population.
-}
substituteParentWithChild' :: [Path] -> [(Path,Path)] -> [(Path,Path)] -> Int -> [Path]
substituteParentWithChild' xs [] _ _ = xs
substituteParentWithChild' xs _ [] _ = xs
substituteParentWithChild' xs ((a,b):ys) ((c,d):zs) vc =
  let
    s1 = substituteParentWithChild xs a c vc
    s2 = substituteParentWithChild s1 b d vc
  in
    substituteParentWithChild' s2 ys zs vc

{----------------------------------------------------------------------------

                     Mutation Functions

-----------------------------------------------------------------------------}

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

applyMutation :: [Path] -> IO [Path]
applyMutation xs = mapM f xs
                   where
                     f x =
                       do
                        (r1,r2) <- generateTwoPointCrossoverIndices ((length x) -1)
                        pm <- rand 0.0 1.0
                        if (pm <= mutationProbability)
                          then
                            do
                              print (x,r1,r2,pm)
                              return $ swapNodes x (x !! r1) (x !! r2)
                          else return x
                     
