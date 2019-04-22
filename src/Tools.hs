module Tools where

import Data
import Data.List
import Data.List.Split
import Text.Regex.PCRE.Wrap

-- Get persons who matches by name or surnames
getMatches :: Person -> [Person] -> [Person]
getMatches x = filter $ \p -> (regex $ pName x) =~ (pName p) || (regex $ pSurnames x) =~ (pSurnames p)
  where
    regex :: String -> String
    regex = intercalate "|" . splitOn " " . ("(?i)" ++)
