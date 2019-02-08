module Data.MarriageDoc where

import Data.Date
import Data.List.Split
import Data.Time
import Data.Utils
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

getMarriageFromStringList :: [String] -> IO (Maybe MarriageDoc)
getMarriageFromStringList xs = do
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
