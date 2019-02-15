module Data.BirthDoc where

import Data.Date
import Data.List.Split
import Data.Time
import Data.Utils
import Data.UUID

data BirthDoc = Birth
  { bUid            :: UUID
  , bDate           :: Maybe Day
  , bSonUid         :: Maybe UUID
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
  if length xs /= 12
    then return Nothing
    else do
      uid <- getDUid . head $ xs
      return $
        Just
          (Birth
             uid
             (date (xs !! 1))
             (fromString (xs !! 2))
             (xs !! 3)
             (fromString (xs !! 4))
             (xs !! 5)
             (xs !! 6)
             (xs !! 7)
             (fromString (xs !! 8))
             (xs !! 9)
             (xs !! 10)
             (xs !! 11))
