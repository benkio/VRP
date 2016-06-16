module GraphBuilder where

{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Domain

{-
    generate a path diagram
-}
pathToGraph :: Domain.Path -> QDiagram SVG V2 Double Any
pathToGraph p =
  let
    points = pathToPoints p
    nodes = atPoints points $ map node [1..]
    nodesPaired = cnv $ [1..length points] ++ [1]
  in
    foldr (\(x,y) z -> connect x y z ) nodes nodesPaired

{-
    from a path generate a list of points
-}
pathToPoints :: Domain.Path -> [Point V2 Double]
pathToPoints p = map (\y -> p2(coordinatesToDouble(fst y))) p

{-
    Convert coordinate to double due to type checking errors
-}
coordinatesToDouble :: Domain.Coordinate -> (Double, Double)
coordinatesToDouble (x,y) = ((fromIntegral x)/5, (fromIntegral y)/5)

node :: Int -> Diagram B
node n = (text (show n) # fontSizeL 0.2 # fc white <> circle 0.2 # fc green) # named (toName n)

cnv :: [a] -> [(a, a)]
cnv [] = []
cnv (_:[]) = []
cnv (k:v:t) = (k, v) : cnv (v:t)
