module Behaviour.ACO.Ant where

import Control.Monad.State

initialTour :: Path
initialTour = []

{-
    Starting from: the tour and the node
    Return a new state with the two path updated
-}
visitNode :: Path -> Node -> State () Path
visitNode t x
  | x `elem` t  = return t
  | x `notelem` t = return (x : t)

