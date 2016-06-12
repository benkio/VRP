module Parameters where

{------------------------------
         Input Parameters
-------------------------------}

import Domain
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

populationNumber :: Int
populationNumber = 200

vrpInstances :: [Int]
vrpInstances = [0..10]

iterationNumber :: Int
iterationNumber = 500

thrasholdUntilRandomPop :: Int
thrasholdUntilRandomPop = 50

diagramSize :: SizeSpec V2 Double
diagramSize = mkWidth 750.0

{-------------------------------
        TxT Parse Parameters
---------------------------------}

filesBasePath :: VRPFilePath
filesBasePath = "/home/benkio/projects/VRP/files/"

xLineIndex :: Int
xLineIndex = 0

yLineIndex :: Int
yLineIndex = 1

demandLineIndex :: Int
demandLineIndex = 2

{-------------------------------
        Algorithm Parameters
---------------------------------}
crossoverProbability :: Float
crossoverProbability = 0.6

mutationProbability :: Float
mutationProbability = 0.1
