module Parameters where

{------------------------------
         Input Parameters
-------------------------------}

import Domain
import Diagrams.Prelude

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

--  Size of the result diagrams
diagramSize :: SizeSpec V2 Double
diagramSize = mkWidth 750.0

{-------------------------------
        TxT Parse Parameters
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

{-------------------------------
        Algorithm Parameters
---------------------------------}

crossoverProbability :: Float
crossoverProbability = 0.6

mutationProbability :: Float
mutationProbability = 0.1
