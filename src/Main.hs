module Main where

import Control.Monad
import Parser
import Data
import Uid
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
  persons <- return $ getPersons (prepare births) (prepare marriages) (prepare deaths)
  result <- setUuid persons
  if hasAllArgs && filesExists
    then putStrLn $ show result
    else putStrLn "Three valid files needed as arguments (birthsFile, marriagesFile, deathsFile)"
  where
    exists = fmap and . mapM doesFileExist
    checkArgs = return . (== 3) . length

prepare :: [Maybe x] -> [x]
prepare = (map extractM . filter isValidRow)
  where
    extractM :: Maybe x -> x
    extractM (Just x) = x
    isValidRow :: Maybe x -> Bool
    isValidRow (Just x) = True
    isValidRow Nothing  = False
    