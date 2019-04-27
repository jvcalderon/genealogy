module Uid where

import Data
import Data.UUID.V4

setUuid = setUuidToAllDeaths

-- Each person just can die once
setUuidToAllDeaths :: [Person] -> IO [Person]
setUuidToAllDeaths xs = do
  sequence $ map setUid xs
  where
    setUid x = do
      uuid <- nextRandom
      if (pRole x == Deceased && pUid x == Nothing)
        then return (x {pUid = Just uuid})
        else return x
