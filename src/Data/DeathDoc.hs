module Data.DeathDoc where

import Data.List.Split
import Data.Time
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
