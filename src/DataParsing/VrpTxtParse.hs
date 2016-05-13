module DataParsing.VrpTxtParse where

-- This file will try to parse a txt for the vrp problem from 
-- http://people.brunel.ac.uk/~mastjjb/jeb/orlib/files/

type FileName = String

getInstanceFiles :: Int ->[String]
getInstanceFiles 0 = ["vrpnc.txt"]
getInstanceFiles x = ("vrpnc"++ (show x) ++".txt") : ( getInstanceFiles $ x-1 )


readSingleFile :: FileName -> IO String
readSingleFile f = do readFile f