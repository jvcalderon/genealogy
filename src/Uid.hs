module Uid where

import Data
import Data.UUID.V4
import Matcher

assign :: [Person] -> Person -> IO Person
assign xs x = do
  uuid <- nextRandom
  matches <- return $ getMatches x xs
  matchCount <- return . length $ matches
  if matchCount == 0 && pUid x == Nothing
    then return (x {pUid = Just uuid})
    else if matchCount == 1 && pUid x == Nothing
           then return (x {pUid = pUid $ head matches})
           else return x

setUids :: [Person] -> IO [Person]
setUids xs = do
  assignAll [] xs
  where
    assignAll :: [Person] -> [Person] -> IO [Person]
    assignAll xs [] = return xs
    assignAll processed remain = do
      uid <- return . pUid . head $ remain
      x <- assign (processed ++ tail remain) (head remain)
      if uid == Nothing && pUid x /= Nothing
        then assignAll [] (processed ++ tail remain ++ [x]) -- If uuid is assigned, check process must start from the beginning
        else assignAll (x : processed) (tail remain)
