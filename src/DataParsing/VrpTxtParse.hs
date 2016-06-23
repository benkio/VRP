module DataParsing.VrpTxtParse where

{-
    This file will try to parse a txt for the vrp problem from
    http://people.brunel.ac.uk/~mastjjb/jeb/orlib/files/

    All the data can be easely estracted with the following functions
-}
import Errors
import Domain
import Parameters

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

{-
    This funcion return the node list.
    if the vehicle capacity is too low it split che nodes in multiple lists
-}
nodes :: VRPFileContent -> Int ->[[Node]]
nodes c vc = groupNodesByCapacity ( map (\s -> (((readElementFromLine s xLineIndex), (readElementFromLine s yLineIndex)), (readElementFromLine s demandLineIndex))) l) vc
  where
    l = tail $ tail $ lines c

groupNodesByCapacity :: [Node] -> Int -> [[Node]]
groupNodesByCapacity [] _ = []
groupNodesByCapacity ns vc =
  let
    (ns', rest) = takeOneByCapacity ns vc []
  in
    if rest /= []
    then ns' : groupNodesByCapacity rest vc
    else [ns']


takeOneByCapacity :: [Node] -> Int -> [Node] -> ([Node], [Node])
takeOneByCapacity [] _ acc = (acc, [])
takeOneByCapacity (x:xs) vc acc
  | vc > 0 = if snd x < vc
             then takeOneByCapacity xs (vc - snd x) $ x:acc
             else takeOneByCapacity (x:xs) 0 acc
  | otherwise =  (acc, xs)

getNode :: [Node] -> Int -> Node
getNode ns x = ns !! x

getNodeDemand :: [Node] -> Int -> Demand
getNodeDemand ns x = snd $ getNode ns x

getNodeCoordinates :: [Node] -> Int -> Coordinate
getNodeCoordinates ns x = fst $ getNode ns x

getNodeXCoordinate :: [Node] -> Int -> Int
getNodeXCoordinate ns x = fst $ getNodeCoordinates ns x

getNodeYCoordinate :: [Node] -> Int -> Int
getNodeYCoordinate ns x = snd $ getNodeCoordinates ns x
