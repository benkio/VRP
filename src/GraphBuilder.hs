module GraphBuilder where

{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Arrow
import Diagrams.TwoD.Text
import Domain

{-
    generate a path diagram
-}
pathToGraph :: Domain.Path -> Diagram B
pathToGraph p = atPoints (pathToPoints p) (map node [1..])

{-
    from a path generate a list of points
-}
pathToPoints :: Domain.Path -> [Point V2 Double]
pathToPoints p = map (\y -> p2(coordinatesToDouble(fst y))) p

{-
    Convert coordinate to double due to type checking errors
-}
coordinatesToDouble :: Domain.Coordinate -> (Double, Double)
coordinatesToDouble (x,y) = (fromIntegral x, fromIntegral y)

node :: Int -> Diagram B
node n = text (show n) # fontSizeL 0.2 # fc white <> circle 0.2 # fc green
