module Behaviour.Genetics.Algorithm where

{-
    This file contains the algorithm of the generics for the VRP
-}

import Domain
import Data.Random
import System.Random
import Data.List
import Behaviour.NodeAndPathCalculator
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
    Generate a random path. First try a random shuffle sequence of node from the ones in input
    After a 100 recursion calls none is generated the head of the nodes will be removed.
    And all start form the beginning.
-}
generateRandomPath :: [Node] -> Bool -> Path -> Int -> RVar Path
generateRandomPath _ True finalValue _ = return finalValue
generateRandomPath [] _ _ _ = return []
generateRandomPath nodes False _ veicleCapacity = do
                                         s <- shuffle nodes
                                         let s' = ((0,0),0) : s
                                         generateRandomPath nodes (validator veicleCapacity s') s' veicleCapacity

{-
    Use the previous function to generate a fixed number of random paths
    It tries indefinitely CANNOT RETURN IF THE NUMBER OF PATH CANNOT BE GENERATED.
    the return avoid duplications and empty strings
-}
generateRandomPaths :: (Eq a, Num a) => a -> [Path] -> [Node] -> Int -> RVar [Path]
generateRandomPaths 0 acc _ _ = return acc
generateRandomPaths n acc nodes veicleCapacity = do
                            v <- generateRandomPath nodes False [] veicleCapacity
                            if (v `elem` acc)
                            then generateRandomPaths n acc nodes veicleCapacity
                            else generateRandomPaths (n-1) (v:acc) nodes veicleCapacity

{--------------------------------------------------------------------------------------

                         MONTECARLO EXTRACTION FUNCITONS

 ---------------------------------------------------------------------------------------}

{-
     Do a montecarlo selection on the input population and return a new population
-}
singleMontecarloExtraction :: Float -> [(Path,Float)] -> IO Path
singleMontecarloExtraction tf ranges =
  do
    r <- rand 0.0 tf
    return $ f r ranges
  where
    f :: Float -> [(Path,Float)] -> Path
    f _ [] = []
    f _ (z:[]) = fst z
    f pick (z:zs) =
      if ((snd z) > pick)
      then fst z
      else f pick zs



-- Function that do the montecarlo, uses the previous function and concat single montecarlo extraction
montecarlo :: [Path] -> Int -> IO [Path]
montecarlo x y = sequence $ f y
                 where
                   tf = totalFitness x
                   ranges = zip x $ montecarloRages x 0
                   f 0 = do []
                   f n = singleMontecarloExtraction tf ranges : f (n-1)

{-
    From the input population fitness return a list of ranges from 1 to the total fitness
-}
montecarloRages :: [Path] -> Float -> [Float]
montecarloRages [] _ = []
montecarloRages paths acc = x : montecarloRages (tail paths) x
                                where
                                  x = acc + head (fitnessInverse paths)

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
pathPair [] = []
pathPair (x:xs) = pathPairBuilder x xs ++ pathPair xs

{-
    From a list of floats(randoms) and a list of pairs of paths
    return a list of pairs of paths filtered by the crossover probability
-}
selectPathByFloat :: [Float] -> [Path] -> [Path] -> [Path]
selectPathByFloat _ [] acc = acc
selectPathByFloat [] _ acc = acc
selectPathByFloat (x:xs) (y:ys) acc = if (x <= crossoverProbability)
                                      then selectPathByFloat xs ys (y : acc)
                                      else selectPathByFloat xs ys acc

{-
    From a list of paths it return the crossover selection. see previous functions
-}
selectForCrossOver :: [Path] -> IO [(Path, Path)]
selectForCrossOver [] = return []
selectForCrossOver xs =
    do
      r <- randList 0.0 1.0 (length xs)
      r' <- randList 0.0 1.0 (length xs)
      let selectedPaths = selectPathByFloat r (xs) []
      let selectedPaths' = selectPathByFloat r' (xs) []
    --  print r
      return $ filterSimmetric (filter (\(x,y) -> x /= y) (zip selectedPaths selectedPaths')) []

filterSimmetric :: [(Path,Path)] -> [(Path,Path)] -> [(Path,Path)]
filterSimmetric [] acc = acc
filterSimmetric ((x,y):xs) acc = if ( (x,y) `notElem` acc && (y,x) `notElem` acc )
                                    then filterSimmetric xs $ (x,y) : acc
                                    else filterSimmetric xs acc

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
    | validator vc c = substitute xs (worsePathFun xs) c
    | otherwise = xs

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
                            --  print (x,r1,r2,pm)
                              return $ swapNodes x (x !! r1) (x !! r2)
                          else return x
