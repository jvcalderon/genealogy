module Parser
  ( getBirth
  , getMarriage
  ) where

import Data
import Data.List.Split
import Data.UUID
import Data.UUID.V4

getLines :: IO [[String]]
getLines = do
  x <- getLine
  if x == ""
    then return []
    else do
      xs <- getLines
      return ((splitOn ";" x) : xs)

getBirth :: [String] -> IO (Maybe BirthDoc)
getBirth xs = do
  if length xs /= 11
    then return Nothing
    else do
      uid <- getDUid . head $ xs
      return $
        Just
          (Birth
             uid
             (date (xs !! 1))
             (xs !! 2)
             (fromString (xs !! 3))
             (xs !! 4)
             (xs !! 5)
             (xs !! 6)
             (fromString (xs !! 7))
             (xs !! 8)
             (xs !! 9)
             (xs !! 10))

getMarriage :: [String] -> IO (Maybe MarriageDoc)
getMarriage xs = do
  if length xs /= 10
    then return Nothing
    else do
      uid <- getDUid . head $ xs
      return $
        Just
          (Marriage
             uid
             (date (xs !! 1))
             (fromString (xs !! 2))
             (xs !! 3)
             (xs !! 4)
             (xs !! 5)
             (fromString (xs !! 6))
             (xs !! 7)
             (xs !! 8)
             (xs !! 9))

docUid :: String -> Either (IO UUID) (Maybe UUID)
docUid x
  | (x == "") = Left nextRandom
  | otherwise = Right (fromString x)

lift :: Either (IO UUID) (Maybe UUID) -> IO UUID
lift (Right (Just x)) = return x
lift (Right Nothing)  = nextRandom
lift (Left x)         = x

getDUid :: String -> IO UUID
getDUid = lift . docUid
