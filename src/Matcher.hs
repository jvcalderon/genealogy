module Matcher where

import Data
import Data.List
import Data.List.Split
import Data.UUID.V4
import Text.Regex.PCRE.Wrap

getMatches :: Person -> [Person] -> [Person]
getMatches x = justOneDie x . matchesByNameOrSurname x

-- People can't die twice
justOneDie :: Person -> [Person] -> [Person]
justOneDie x = filter (\p -> pRole p /= Deceased || pRole x /= Deceased)

-- Get people who matches by name or surnames
matchesByNameOrSurname :: Person -> [Person] -> [Person]
matchesByNameOrSurname x xs = filter (\p -> name x =~ name p || surnames x =~ surnames p) (withUid xs)
  where
    regex :: String -> String
    regex = intercalate "|" . splitOn " " . ("(?i)" ++)
    sanitize :: String -> String
    sanitize = filter (not . (`elem` ",.?!-:;\"\'"))
    name :: Person -> [Char]
    name = regex . sanitize . pName
    surnames :: Person -> [Char]
    surnames = regex . sanitize . pSurnames
    withUid :: [Person] -> [Person]
    withUid = filter $ \x -> pUid x /= Nothing