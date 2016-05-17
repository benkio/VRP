module Domain where

{-
    This file contains all the type definition for the
    custom types based on the VRP problem
-}

type VRPFileName    = String
type VRPFileContent = String
type VRPFilePath    = String
type VRPFileLine    = String
type Coordinate  = (Int,Int)
type Demand      = Int
type Node        = (Coordinate, Demand)
type Path        = [Node]
