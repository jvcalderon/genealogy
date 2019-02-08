module Data.DeathDoc where

import Data.Date
import Data.List.Split
import Data.Time
import Data.Utils
import Data.UUID

data DeathDoc = Death
  { dUid          :: UUID
  , dPersonUid    :: Maybe UUID
  , dDate         :: Maybe Day
  , dName         :: String
  , dSurname      :: String
  , dYear         :: Maybe Int
  , dCause        :: String
  , dAge          :: Maybe Int
  , dJob          :: String
  , dTestament    :: String
  , dMunicipality :: String
  , dDescription  :: String
  } deriving (Show)

instance Eq DeathDoc where
  d1 == d2 = (dUid d1) == (dUid d2)
  d1 /= d2 = (dUid d1) /= (dUid d2)

getDeathFromStringList :: [String] -> IO (Maybe DeathDoc)
getDeathFromStringList xs = do
  if length xs /= 12
    then return Nothing
    else do
      uid <- getDUid . head $ xs
      return $
        Just
          (Death
             uid
             (fromString $ xs !! 1)
             (date $ xs !! 2)
             (xs !! 3)
             (xs !! 4)
             (getInt $ xs !! 5)
             (xs !! 6)
             (getInt $ xs !! 7)
             (xs !! 8)
             (xs !! 9)
             (xs !! 10)
             (xs !! 11))
