module Data.Date where

import Data.Time

date :: String -> Maybe Day
date x = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" x :: Maybe Day
