module Parameters where

import Domain
import Diagrams.Prelude

{------------------------------
         Genetics Parameters
-------------------------------}

-- Number of chromosomes/ants in the algorithms
-- The limit of the population number is 7!
populationNumber :: Int
populationNumber = 200

{-
    Number of iteration/recursion in the algorithm until termination for every instance.
-}
iterationNumber :: Int
iterationNumber = 1000

{-
    If the number of consecutively iteration where the best result
    doesn't change exceed this thrashold, the population
    will be randomly regenerated.
-}
thrasholdUntilRandomPop :: Int
thrasholdUntilRandomPop = 250

{-
    Probability todo the crossover
-}
crossoverProbability :: Float
crossoverProbability = 0.6

{-
    Probability todo the mutation
-}
mutationProbability :: Float
mutationProbability = 0.1

{-----------------------------------------
    ACO Parameters
-------------------------------------------}

antNumber :: Int
antNumber = 10

-- Importance of the attactivity over pheromone trace
alfa :: Int
alfa = 1

-- Importance of the pheromone trace over attractivity
beta :: Int
beta = 5

evaporationCoefficient :: Float
evaporationCoefficient = 0.8

initialPheromoneTrace :: Float
initialPheromoneTrace = 1.0

{-------------------------------
        TxT Parse and others Parameters
---------------------------------}

-- PATH TO SET for the location of the instances.
filesBasePath :: VRPFilePath
filesBasePath = "/home/benkio/projects/VRP/files/"

-- Index of the X coordinate
xLineIndex :: Int
xLineIndex = 0

-- Index of the Y coordinate
yLineIndex :: Int
yLineIndex = 1

-- Index of the demand value for each node.
demandLineIndex :: Int
demandLineIndex = 2

--  Size of the result diagrams
diagramSize :: SizeSpec V2 Double
diagramSize = mkWidth 750.0
