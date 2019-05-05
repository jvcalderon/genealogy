module Data.Person where

import Data.BirthDoc
import Data.Date
import Data.DeathDoc
import Data.List.Split
import Data.MarriageDoc
import Data.Text        hiding (concat, map, filter, head, splitOn)
import Data.Time
import Data.Utils
import Data.UUID

data Role
  = Son
  | Father
  | Mother
  | Deceased
  | Bridegroom
  deriving (Show, Eq)

data Person = Person
  { pUid      :: Maybe UUID
  , pDocUid   :: UUID
  , pDate     :: Maybe Day
  , pName     :: String
  , pSurnames :: String
  , pNickName :: String
  , pRole     :: Role
  } deriving (Show)

getPersonsInBirthDoc :: BirthDoc -> [Person]
getPersonsInBirthDoc x =
  [ Person (bSonUid x) (bUid x) (bDate x) (bName x) (surnames (bFatherSurname1 x) (bMotherSurname x)) "" Son
  , Person
      (bFatherUid x)
      (bUid x)
      (bDate x)
      (bFatherName x)
      (surnames (bFatherSurname1 x) (bFatherSurname2 x))
      ""
      Father
  , Person (bMotherUid x) (bUid x) (bDate x) (bMotherName x) (bMotherSurname x) (bMotherNickname x) Mother
  ]

getPersonsInMarriageDoc :: MarriageDoc -> [Person]
getPersonsInMarriageDoc x =
  [ Person (mUid1 x) (mUid x) (mDate x) (mName1 x) (surnames (mSurname11 x) (mSurname12 x)) "" Bridegroom
  , Person (mUid2 x) (mUid x) (mDate x) (mName2 x) (surnames (mSurname21 x) (mSurname22 x)) "" Bridegroom
  ]

getPersonsInDeathDoc :: DeathDoc -> [Person]
getPersonsInDeathDoc x = [Person (dPersonUid x) (dUid x) (dDate x) (dName x) (dSurname x) "" Deceased]

getPersons :: [BirthDoc] -> [MarriageDoc] -> [DeathDoc] -> [Person]
getPersons bs ms ds =
  concat [(bs >>= getPersonsInBirthDoc), (ms >>= getPersonsInMarriageDoc), (ds >>= getPersonsInDeathDoc)]

surnames :: String -> String -> String
surnames a b = unpack . strip . pack $ a ++ " " ++ b

getDeathsFromPersonList :: [Person] -> [DeathDoc]
getDeathsFromPersonList = map (mapper) . filter (\x -> pRole x == Deceased)
  where
    year :: Person -> Int
    year = (\x -> read x :: Int) . head . splitOn "-" . showGregorian . extractM . pDate
    extractM :: Maybe x -> x
    extractM (Just x) = x
    mapper :: Person -> DeathDoc
    mapper x = Death (pDocUid x) (pUid x) (pDate x) (pName x) (pSurnames x) (Just $ year x) "" Nothing "" "" "" ""
