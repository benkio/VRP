module Errors where

{-
    Custom datatypes for the errors
-}

data CustomError = NegativeIndex
                 | MiscError String
                 | UnexpectedInput

instance Show CustomError where
  show NegativeIndex = "Negative Index Not Supported"
  show UnexpectedInput = "Unexpected Input"
  show (MiscError str) = str
