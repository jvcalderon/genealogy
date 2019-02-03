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
  putStrLn $
    if hasAllArgs && filesExists
      then "Ok"
      else "Three valid files needed as arguments (birthsFile, marriagesFile, deathsFile)"
  where
    exists = fmap and . mapM doesFileExist
    checkArgs = return . (== 3) . length
      {-csvFile :: GenParser Char st [[String]]
csvFile = do
  result <- many line
  eof
  return result-}
{-

type GenParser tok st = Parsec [tok] st

line :: GenParser Char st [String]
line = do
  result <- cells
  eol
  return result

cells :: GenParser Char st [String]
cells = do
  first <- cellContent
  next <- remainingCells
  return (first : next)

remainingCells :: GenParser Char st [String]
remainingCells =
  (char ';' >> cells) -- Found comma?  More cells coming
   <|>
  (return []) -- No comma?  Return [], no more cells

cellContent :: GenParser Char st String
cellContent = many (noneOf ",\n")

eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
-}
