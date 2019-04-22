module Parser where

import Data
import Data.List.Split
import Data.UUID
import Data.UUID.V4
import System.Directory

getContent :: ([String] -> d) -> FilePath -> IO [d]
getContent typeConstruct path = do
  xs <- toChar . getLines $ path
  return $ map typeConstruct (toString (fmap (splitOn ";") xs))
  where
    getLines = fmap lines . readFile
    toChar = fmap (fmap (\x -> x :: [Char]))
    toString = map (map (\x -> x :: String))

getBirths :: FilePath -> IO [Maybe BirthDoc]
getBirths fp = do
  xs <- getContent getBirthFromStringList fp
  sequence xs

getMarriages :: FilePath -> IO [Maybe MarriageDoc]
getMarriages fp = do
  xs <- getContent getMarriageFromStringList fp
  sequence xs

getDeaths :: FilePath -> IO [Maybe DeathDoc]
getDeaths fp = do
  xs <- getContent getDeathFromStringList fp
  sequence xs
