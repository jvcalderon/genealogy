module Parser where

import Data
import Data.List.Split
import Data.UUID
import Data.UUID.V4
import System.Directory
import Text.Regex.PCRE

getContent :: ([String] -> d) -> FilePath -> IO [d]
getContent typeConstruct path = do
  xs <- toChar . getLines $ path
  return $ map typeConstruct (toString (fmap (splitOn ";") xs))
  where
    getLines = fmap lines . readFile
    toChar = fmap (fmap (\x -> x :: [Char]))
    toString = map (map (\x -> x :: String))

getBirths :: FilePath -> IO [IO (Maybe BirthDoc)]
getBirths = getContent getBirthFromStringList

getMarriages :: FilePath -> IO [IO (Maybe MarriageDoc)]
getMarriages = getContent getMarriageFromStringList

getDeaths :: FilePath -> IO [IO (Maybe DeathDoc)]
getDeaths = getContent getDeathFromStringList
