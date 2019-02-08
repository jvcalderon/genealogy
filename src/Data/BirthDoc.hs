module Data.BirthDoc where

import Data.Date
import Data.List.Split
import Data.Time
import Data.Utils
import Data.UUID

data BirthDoc = Birth
  { bUid            :: UUID
  , bDate           :: Maybe Day
  , bName           :: String
  , bFatherUid      :: Maybe UUID
  , bFatherName     :: String
  , bFatherSurname1 :: String
  , bFatherSurname2 :: String
  , bMotherUid      :: Maybe UUID
  , bMotherName     :: String
  , bMotherSurname  :: String
  , bMotherNickname :: String
  } deriving (Show)

instance Eq BirthDoc where
  b1 == b2 = (bUid b1) == (bUid b2)
  b1 /= b2 = (bUid b1) /= (bUid b2)

getBirthFromStringList :: [String] -> IO (Maybe BirthDoc)
getBirthFromStringList xs = do
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
