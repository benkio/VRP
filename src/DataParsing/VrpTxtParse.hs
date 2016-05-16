module DataParsing.VrpTxtParse where

-- This file will try to parse a txt for the vrp problem from
-- http://people.brunel.ac.uk/~mastjjb/jeb/orlib/files/

import Errors
import Domain

filesBasePath :: VRPFilePath
filesBasePath = "/home/benkio/projects/VRP/files/"

xLineIndex :: Int
xLineIndex = 0

yLineIndex :: Int
yLineIndex = 1

demandLineIndex :: Int
demandLineIndex = 2

getFileNames :: Int -> Either CustomError [VRPFileName]
getFileNames x | x > 0 = Right $ ("vrpnc"++ (show x) ++".txt") : ( getInstanceFiles (x-1))
               | x < 0 = Left NegativeIndex
               | x == 0 = Right $ ["vrpnc.txt"]
getFileNames _ = Left UnexpectedInput

getInstanceFiles :: Int -> [VRPFileName]
getInstanceFiles x = case (getFileNames x) of
                       Left err  -> error (show err)
                       Right files -> map (filesBasePath++) files

readSingleFile :: VRPFileName -> IO VRPFileContent
readSingleFile f = do readFile f

readElementFromLine :: VRPFileLine -> Int -> Int
readElementFromLine l x = read ((words l) !! x) :: Int

readElementFromFile :: VRPFileContent -> Int -> Int -> Int
readElementFromFile c x y = readElementFromLine ((lines c) !! x) y

getXCoordinate :: VRPFileContent -> Int -> Int
getXCoordinate c l = readElementFromFile c l xLineIndex

getYCoordinate :: VRPFileContent -> Int -> Int
getYCoordinate c l = readElementFromFile c l yLineIndex

numberOfNodes :: VRPFileContent -> Int
numberOfNodes c = readElementFromFile c 0 xLineIndex

vehiclesCapacity :: VRPFileContent -> Int
vehiclesCapacity c = readElementFromFile c 0 2

startingPoint :: VRPFileContent -> Coordinate
startingPoint c = ((getXCoordinate c 1), (getYCoordinate c 1))

nodes :: VRPFileContent -> [Node]
nodes c = map (\s -> (((readElementFromLine s xLineIndex), (readElementFromLine s yLineIndex)), (readElementFromLine s demandLineIndex))) l
        where
          l = tail $ tail $ lines c

getNode :: VRPFileContent -> Int -> Node
getNode c x = (nodes c) !! x

getNodeDemand :: VRPFileContent -> Int -> Demand
getNodeDemand c x = snd $ getNode c x

getNodeCoordinates :: VRPFileContent -> Int -> Coordinate
getNodeCoordinates c x = fst $ getNode c x

getNodeXCoordinate :: VRPFileContent -> Int -> Int
getNodeXCoordinate c x = fst $ getNodeCoordinates c x

getNodeYCoordinate :: VRPFileContent -> Int -> Int
getNodeYCoordinate c x = snd $ getNodeCoordinates c x
