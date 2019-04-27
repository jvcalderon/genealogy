module Matcher where

import Data
import Data.List
import Data.List.Split
import Data.UUID.V4
import Text.Regex.PCRE.Wrap

-- Get persons who matches by name or surnames
getMatches :: Person -> [Person] -> [Person]
getMatches x = filter $ \p -> name x =~ name p && surnames x =~ surnames p
  where
    regex :: String -> String
    regex = intercalate "|" . splitOn " " . ("(?i)" ++)
    sanitize :: String -> String
    sanitize = filter (not . (`elem` ",.?!-:;\"\'"))
    name :: Person -> [Char]
    name = regex . sanitize . pName
    surnames :: Person -> [Char]
    surnames = regex . sanitize . pSurnames
