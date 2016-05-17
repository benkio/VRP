module Main where

import DataParsing.VrpTxtParse --add to force compilation
import Behaviour.NodeAndPathCalculator
import Behaviour.Genetics.Algorithm

main :: IO()
main = do
  fileContent <- readSingleFile $ head $ getInstanceFiles $ 0
  printRVar (generateRandomPaths 3 [] (nodes fileContent) 10)
