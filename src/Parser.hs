module Parser where

import Data
import Data.List.Split
import Data.UUID
import Data.UUID.V4
import Text.Regex.PCRE

getLines :: IO [[String]]
getLines = do
  x <- getLine
  if x == ""
    then return []
    else do
      xs <- getLines
      return ((splitOn ";" x) : xs)
