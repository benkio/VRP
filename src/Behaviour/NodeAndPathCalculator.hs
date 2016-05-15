module Behaviour.NodeAndPathCalculator where

import Domain

pow :: Num x => x -> x
pow x = x * x

calculateDistance :: Coordinate -> Coordinate -> Float
calculateDistance (x,y) (a,b) = sqrt ( c + d )
                              where
                                c = fromIntegral $ pow $ x-a
                                d = fromIntegral $ pow $ y-b

calculatePathDistance :: [Coordinate] -> Float
calculatePathDistance [] = 0
calculatePathDistance (x:y:xs) = (calculateDistance x y) + calculatePathDistance (y:xs)
calculatePathDistance (_:[]) = 0

calculateDemand :: [Demand] -> Int
calculateDemand x = foldr (+) 0 x

pathIsValid :: Int -> [Demand] -> Bool
pathIsValid x y = (calculateDemand y) < x
