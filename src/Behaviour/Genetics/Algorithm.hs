module Behaviour.Genetics.Algorithm where

import Domain
import System.Random
import Data.Random
import Data.Random.RVar
import Data.List
import Behaviour.NodeAndPathCalculator

validator :: Int -> [Node] -> Bool
validator vc xs = (pathIsValid vc (map snd xs)) && xs/=[]

generateValidPopulation :: [Node] -> Int -> ([Node] -> [Path]) -> [Path]
generateValidPopulation xs vc gen = [ b | b <- gen xs,
                             -- b <- subsequences a,
                              validator vc b]

validPopulation :: [Node] -> Int -> [Path]
validPopulation xs vc = generateValidPopulation xs vc permutations

generateRandomPath :: [Node] -> RVar Path
generateRandomPath xs = shuffle xs

generateRandomPaths :: (Eq a, Num a) => a -> [Path] -> [Node] -> RVar [Path]
generateRandomPaths 0 acc _ = return acc
generateRandomPaths n acc xs = do
                            v <- generateRandomPath xs
                            generateRandomPaths (n-1) (v : acc) xs

generateInitialPopulation :: MonadRandom m => [Node] -> Int -> Int -> [Path] -> m [Path]
generateInitialPopulation _ 0 _ acc = return acc
generateInitialPopulation xs n vc acc = do
                                          paths <- (runRVar (generateRandomPaths n [] xs) StdRandom)
                                          generateInitialPopulation xs (n - (length (pathFiltered paths))) vc ((pathFiltered paths)  ++ acc)
                                          where
                                            pathFiltered xs = filter (validator vc) (foldr (:) [] xs)
