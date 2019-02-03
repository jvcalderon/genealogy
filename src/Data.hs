module Data
  ( BirthDoc(Birth, bUid, bDate, bName, bFatherUid, bFatherName,
         bFatherSurname1, bFatherSurname2, bMotherUid, bMotherName,
         bMotherSurname, bMotherNickname)
  , MarriageDoc(Marriage, mUid, mDate, mUid1, mName1, mSurname11,
            mSurname12, mUid2, mName2, mSurname21, mSurname22)
  , DeathDoc
  , date
  ) where

import Data.List.Split
import Data.Time
import Data.UUID

date :: String -> Maybe Day
date x = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" x :: Maybe Day

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

data MarriageDoc = Marriage
  { mUid       :: UUID
  , mDate      :: Maybe Day
  , mUid1      :: Maybe UUID
  , mName1     :: String
  , mSurname11 :: String
  , mSurname12 :: String
  , mUid2      :: Maybe UUID
  , mName2     :: String
  , mSurname21 :: String
  , mSurname22 :: String
  } deriving (Show)
  
instance Eq MarriageDoc where
  b1 == b2 = (mUid b1) == (mUid b2)
  b1 /= b2 = (mUid b1) /= (mUid b2)

data DeathDoc = Death
  { dUid          :: UUID
  , dPersonUid    :: UUID
  , dDate         :: Day
  , dName         :: String
  , dSurname      :: String
  , dYear         :: Int
  , dCause        :: String
  , dAge          :: Int
  , dJob          :: String
  , dTestament    :: String
  , dMunicipality :: String
  , dDescription  :: String
  } deriving (Show)
