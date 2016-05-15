module Main where

import DataParsing.VrpTxtParse --add to force compilation

main :: IO()
main = do
  fileContent <- readSingleFile $ head $ getInstanceFiles $ 0
  print fileContent
