module Behaviour.ACO.Solver where

import Control.Monad.State
import Domain
import Parameters


startingTrace :: [Node] -> [(Node, Float)]
startingTrace ns = [ (n,initialPheromoneTrace) | n <- ns]
