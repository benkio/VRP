module Domain where

type VRPFileName    = String
type VRPFileContent = String
type VRPFilePath    = String
type VRPFileLine    = String
type Coordinate  = (Int,Int)
type Demand      = Int
type Node        = (Coordinate, Demand)
type Path        = [Node]
