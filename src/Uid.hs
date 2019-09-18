module Uid where

import Data
import Data.UUID.V4
import Matcher
import System.ProgressBar
import Log as L

assign :: [Person] -> Person -> IO Person
assign xs x = do
  uuid <- nextRandom
  matches <- return $ getMatches x xs
  matchCount <- return . length $ matches
  L.log $ "Processing " ++ pName x ++ " " ++ pSurnames x
  if matchCount == 0 && pUid x == Nothing
    then return (x {pUid = Just uuid})
    else if matchCount == 1 && pUid x == Nothing
           then return (x {pUid = pUid $ head matches})
           else return x

setUids :: [Person] -> IO [Person]
setUids xs = do
  (pr, _) <- startProgress percentage exact 50 (Progress 0 (toInteger . length $ xs))
  L.reset
  assignAll [] xs pr 0
  where
    assignAll :: [Person] -> [Person] -> ProgressRef -> Int -> IO [Person]
    assignAll xs [] _ _ = return xs
    assignAll processed remain pr count = do
      uid <- return . pUid . head $ remain
      x <- assign (processed ++ tail remain) (head remain)
      c <-
        if count < length processed
          then do
            incProgress pr 1
            return $ count + 1
          else return count
      if uid == Nothing && pUid x /= Nothing
        then assignAll [] (processed ++ tail remain ++ [x]) pr c -- If uuid is assigned, check process must start from the beginning
        else assignAll (x : processed) (tail remain) pr c
