module Main where

import Control.Monad
import Parser
import System.Directory
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  hasAllArgs <- checkArgs args
  filesExists <- exists args
  births <- getBirths $ args !! 0
  marriages <- getMarriages $ args !! 1
  deaths <- getDeaths $ args !! 2
  if hasAllArgs && filesExists
    then putStrLn "OOOK"
    else putStrLn "Three valid files needed as arguments (birthsFile, marriagesFile, deathsFile)"
  where
    exists = fmap and . mapM doesFileExist
    checkArgs = return . (== 3) . length

prepare :: [IO (Maybe x)] -> IO ([x])
prepare xs = do
  rows <- sequence xs
  return $ (map extractM . filter isValidRow) rows
  where
    extractM :: Maybe x -> x
    extractM (Just x) = x
    isValidRow :: Maybe x -> Bool
    isValidRow (Just x) = True
    isValidRow Nothing  = False
