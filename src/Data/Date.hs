module Data.Date where

import Data.Time

date :: String -> Maybe Day
date x = parseTimeM True defaultTimeLocale "%-d/%-m/%Y" x :: Maybe Day

dateStr :: Day -> String
dateStr = formatTime defaultTimeLocale "%d/%m/%Y"
