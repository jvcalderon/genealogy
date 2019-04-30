module Matcher where

import Data
import Data.Date
import Data.List
import Data.List.Split
import Data.Time
import Data.UUID.V4
import Text.Regex.PCRE.Wrap

getMatches :: Person -> [Person] -> [Person]
getMatches x = inCronOrder x . justOnceInDoc x . justOneBorn x . justOneDie x . matchesByNameOrSurname x

once :: Role -> Person -> [Person] -> [Person]
once r p = filter (\x -> pRole x /= r || pRole p /= r)

-- People can't die twice
justOneDie :: Person -> [Person] -> [Person]
justOneDie = once Deceased

-- People can't born twice
justOneBorn :: Person -> [Person] -> [Person]
justOneBorn = once Son

-- People just can appear once in the same document
justOnceInDoc :: Person -> [Person] -> [Person]
justOnceInDoc x = filter $ \p -> pDocUid p /= pDocUid x

-- People can't be in documents before their birth date or after their death
inCronOrder :: Person -> [Person] -> [Person]
inCronOrder x = filter $ inCronFilter x
  where
    docDate :: Maybe Day -> Day
    docDate (Just x) = x
    docDate Nothing  = docDate $ date "10000-01-01"
    inCronFilter :: Person -> Person -> Bool
    inCronFilter a b = birthChecker a b || birthChecker b a || deathChecker a b || deathChecker b a
    checker :: Role -> (Day -> Day -> Bool) -> Person -> Person -> Bool
    checker role fn a b = pRole a == role && fn (docDate $ pDate a) (docDate $ pDate b)
    deathChecker = checker Deceased (>)
    birthChecker = checker Son (<)

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
