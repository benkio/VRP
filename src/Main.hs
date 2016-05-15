module Main where

import DataParsing.VrpTxtParse --add to force compilation
import Behaviour.NodeAndPathCalculator

main :: IO()
main = do
  fileContent <- readSingleFile $ head $ getInstanceFiles $ 0
  print fileContent -- (readElementFromFile fileContent 0 0)
