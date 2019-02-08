module Data.Utils
  ( docUid
  , lift
  , getDUid
  , getInt
  ) where

import Data.UUID
import Data.UUID.V4
import Text.Regex.PCRE

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

getInt :: String -> Maybe Int
getInt x
  | isInt x = Just (read x :: Int)
  | otherwise = Nothing
  where
    isInt :: String -> Bool
    isInt = matchTest (makeRegex "^\\d+$" :: Regex)
