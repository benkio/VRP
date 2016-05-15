module DataParsing.VrpTxtParse where

-- This file will try to parse a txt for the vrp problem from
-- http://people.brunel.ac.uk/~mastjjb/jeb/orlib/files/

import Errors

type FileName = String
type Path = String

filesBasePath :: Path
filesBasePath = "/home/benkio/projects/VRP/files/"

getFileNames :: Int -> Either CustomError [FileName]
getFileNames x | x > 0 = Right $ ("vrpnc"++ (show x) ++".txt") : ( getInstanceFiles (x-1))
               | x < 0 = Left NegativeIndex
               | x == 0 = Right $ ["vrpnc.txt"]
getFileNames _ = Left UnexpectedInput

getInstanceFiles :: Int -> [FileName]
getInstanceFiles x = case (getFileNames x) of
                       Left err  -> error (show err)
                       Right files -> map (filesBasePath++) files

readSingleFile :: FileName -> IO String
readSingleFile f = do readFile f
