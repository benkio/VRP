module Behaviour.NodeAndPathCalculator where

import Domain

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
pathIsValid :: Int -> [Demand] -> Bool
pathIsValid x y = (calculateDemand y) < x


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
validator veicleCapacity nodes = (pathIsValid veicleCapacity (map snd nodes)) && nodes/=[] && not (duplicateCheck nodes)

-- Calculate the fitness of a list of paths
fitness :: [Path] -> [Float]
fitness paths = map (\x -> calculatePathDistance (map fst x)) paths

-- calculate the total fitness of a population
totalFitness :: [Path] -> Float
totalFitness paths = foldr (+) 0 $ fitness $ paths

selectPath :: [Path] -> Path -> (Path->Path->Bool)-> Path
selectPath [] x  _= x
selectPath (x:xs) p f =
    if ( f x p )
    then selectPath xs x f
    else selectPath xs p f

calcFitness :: Path -> Float
calcFitness y = calculatePathDistance (map fst y)
