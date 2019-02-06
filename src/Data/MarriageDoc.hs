module Data.MarriageDoc where

import Data.List.Split
import Data.Time
import Data.UUID

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
  m1 == m2 = (mUid m1) == (mUid m2)
  m1 /= m2 = (mUid m1) /= (mUid m2)
