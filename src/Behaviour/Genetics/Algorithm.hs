module Behaviour.Genetics.Algorithm where

import Domain
import Data.Random
import Data.Random.RVar
import System.Random
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

generateRandomPath :: [Node] -> Bool -> Path -> Int -> Int -> RVar Path
generateRandomPath _ True x _ _ = return x
generateRandomPath [] _ _ _ _ = return []
generateRandomPath xs False _ n vc = if (n==100)
                                     then
                                       generateRandomPath (tail xs) False [] 0 vc
                                     else
                                       (do
                                         s <- shuffle xs
                                         generateRandomPath xs (validator vc s) s (n+1) vc)

generateRandomPaths :: (Eq a, Num a) => a -> [Path] -> [Node] -> Int -> RVar [Path]
generateRandomPaths 0 acc _ _ = return acc
generateRandomPaths n acc xs vc = do
                            v <- generateRandomPath xs False [] 0 vc
                            generateRandomPaths (n-1) (v : acc) xs vc

printRVar :: (Show a) => RVar a -> IO ()
printRVar a = do
                b <- runRVar a StdRandom
                print b
