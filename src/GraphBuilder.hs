module GraphBuilder where

{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

example :: Diagram B
example = flip atPoints (repeat (circle 0.2 # fc green)) $ map p2 $ [(1,1), (0,3), (-2,1), (-1,-4), (2,0)]
