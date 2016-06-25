module Behaviour.NodeAndPathCalculator where

import Domain
import Data.List
import Data.Function

{-
    In this file there're the functions that compute Nodes and Paths
    But not strictly releated with the algorith of solving.
-}

-- return the pow of a number
pow :: Num x => x -> x
pow x = x * x

-- calculate the distance between two coordinates
calculateDistance :: Coordinate -> Coordinate -> Float
calculateDistance (x,y) (a,b) = sqrt ( c + d )
                              where
                                c = fromIntegral $ pow $ x-a
                                d = fromIntegral $ pow $ y-b

-- calculate the distance of a list of coordinates (path)
calculatePathDistance :: [Coordinate] -> Float
calculatePathDistance [] = 0
calculatePathDistance (x:y:xs) = (calculateDistance x y) + calculatePathDistance (y:xs)
calculatePathDistance (_:[]) = 0

-- calculate the demand of a list of demands. (Path)
calculateDemand :: [Demand] -> Int
calculateDemand x = foldr (+) 0 x

-- check if the path is valid (input the vehicle capacity)
demandIsValid :: Int -> [Demand] -> Bool
demandIsValid x y = (calculateDemand y) < x


{-
    Check if in a list is present a duplicate.
-}
duplicateCheck :: (Eq a) => [a] -> Bool
duplicateCheck [] = False
duplicateCheck (x:xs) = if (x `elem` xs)
                          then x `elem` xs
                          else duplicateCheck xs

-- Check if the given Path is valid or not TRUE IF IS VALID
validator :: Int -> Path -> Bool
validator _ [] = False
validator veicleCapacity nodes = (demandIsValid veicleCapacity (map snd nodes)) && not (duplicateCheck nodes) && head nodes == ((0,0),0)

-- Calculate the fitness of a list of paths
fitness :: [Path] -> [Float]
fitness paths = map (\x -> calculatePathDistance (map fst x)) paths

{-
    Calculate the fitness for every path.
    If a path have the snd fitness his fitness will be exchanged with the snd worse...etc
-}
inverse :: [a] -> ([a] -> [Float]) -> [Float]
inverse [] _ = []
inverse xs f =
  let
    ys = sort $ zip (f xs) [0..(length xs)]
    zs = reverse ys
    as = map (\((a,_),(_,d)) -> (a,d)) $ zip ys zs
  in
    map (\x -> fst x) $ sortBy (compare `on` snd) as

-- calculate the total fitness of a population
totalFitness :: [Path] -> Float
totalFitness paths = sum $ fitness $ paths

calcFitness :: Path -> Float
calcFitness [] = 10000000000000 -- escamotage for empty paths
calcFitness y = calculatePathDistance (map fst y)

maximum' :: Ord t => [(t, a)] -> (t, a)
maximum' []     = error "maximum of empty list"
maximum' (x:xs) = maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail (m, n) (p:ps)
          | m < (fst p) = maxTail p ps
          | otherwise   = maxTail (m, n) ps

minimum' :: Ord t => [(t, a)] -> (t, a)
minimum' []     = error "minimum of empty list"
minimum' (x:xs) = maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail (m, n) (p:ps)
          | m > (fst p) = maxTail p ps
          | otherwise   = maxTail (m, n) ps


worsePathFun :: [Path] -> Path
worsePathFun xs = snd $ maximum' $ zip (map (calcFitness) xs) xs

bestPathFun :: [Path] -> Path
bestPathFun xs = snd $ minimum' $ zip (map (calcFitness) xs) xs

pairPathNodes :: [Path] -> [[(Node,Node)]]
pairPathNodes [] = []
pairPathNodes (x:xs) = (zip x (tail x)) : pairPathNodes xs

frequency :: Ord a => [a] -> [(Int,a)] 
frequency list = map (\l -> (length l, head l)) (group (sort list))

getNodePairFrequency :: (Node,Node) -> [(Int,(Node,Node))] -> Int
getNodePairFrequency a ns =
  let
    xs = filter (\(_,b) -> checkNodePairEquality a b) ns
  in
    if null xs
    then 0
    else fst $ head xs

checkNodePairEquality :: (Node,Node) -> (Node,Node) -> Bool
checkNodePairEquality (a,b) (y,z) = (a == y && b == z) || (a == z && b == y)
