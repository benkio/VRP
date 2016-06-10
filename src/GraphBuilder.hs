module GraphBuilder where

{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Domain

pathToGraph :: Domain.Path -> Diagram B
pathToGraph p = atPoints (pathToPoints p) (repeat (circle 0.2 # fc green))

pathToPoints :: Domain.Path -> [Point V2 Double]
pathToPoints p = map (\y -> p2(coordinatesToDouble(fst y))) p

coordinatesToDouble :: Domain.Coordinate -> (Double, Double)
coordinatesToDouble (x,y) = (fromIntegral x, fromIntegral y)
