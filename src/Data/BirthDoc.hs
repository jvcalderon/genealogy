module Data.BirthDoc where

import Data.List.Split
import Data.Time
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
