module Matcher where

import Data
import Data.Date
import Data.List
import Data.List.Split
import Data.Time
import Data.UUID.V4
import Text.Regex.PCRE.Wrap

getMatches :: Person -> [Person] -> [Person]
getMatches x =
  maxLive x .
  incompatibleRoles x .
  inCronOrder x . justOnceInDoc x . justOneBorn x . justOneDie x . matchesByNameOrSurname x . haveUid

docDate :: Maybe Day -> Day
docDate (Just x) = x
docDate Nothing  = docDate $ date "10000-01-01"

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
    inCronFilter :: Person -> Person -> Bool
    inCronFilter a b =
      all (not . hasLimitRoles) [a, b] || birthChecker a b || birthChecker b a || deathChecker a b || deathChecker b a
    checker :: Role -> (Day -> Day -> Bool) -> Person -> Person -> Bool
    checker role fn a b = pRole a == role && fn (docDate $ pDate a) (docDate $ pDate b)
    deathChecker = checker Deceased (>)
    birthChecker = checker Son (<)
    hasLimitRoles x = pRole x == Deceased || pRole x == Son

-- Get people who matches by name or surnames
matchesByNameOrSurname :: Person -> [Person] -> [Person]
matchesByNameOrSurname x xs = filter (\p -> match x p || match p x) (withUid xs)
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
    match :: Person -> Person -> Bool
    match a b = name a =~ name b || surnames a =~ surnames b

-- A father can't be a mother
incompatibleRoles :: Person -> [Person] -> [Person]
incompatibleRoles x = filter $ checker x
  where
    checker :: Person -> Person -> Bool
    checker a b
      | pRole a == Father = pRole b /= Mother
      | pRole a == Mother = pRole b /= Father
      | otherwise = True

-- People don't live more than 110 years
maxLive :: Person -> [Person] -> [Person]
maxLive x = filter $ \p -> (abs $ diffDays (docDate $ pDate p) (docDate $ pDate x)) < 365 * 110

-- Only people with already assigned Uid can be used to match
haveUid :: [Person] -> [Person]
haveUid = filter $ \x -> pUid x /= Nothing
