module Behaviour.ACO.Ant where

import Control.Monad.State
import Domain

initialTour :: Path
initialTour = [((0,0),0)]

{-
    Starting from: the tour and the node
    Return a new state with the two path updated
-}
visitNode :: Path -> Node -> State () Path
visitNode t x
  | x `elem` t  = error "node already visited"
  | x `notElem` t = return (x : t)
  | otherwise = error "strange problem in a exaustive pattern"

isVisited :: Path -> Node -> Bool
isVisited xs n = n `elem` xs
