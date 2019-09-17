module Data.PersonSpec where

import Control.Exception    (evaluate)
import Data
import Data.BirthDocSpec    (consB)
import Data.DeathDocSpec    (consD)
import Data.MarriageDocSpec (consM)
import Data.Time
import Data.UUID
import Parser
import Test.Hspec
import Test.QuickCheck

spec = do
  b <-
    runIO $
    getBirthFromStringList
      (map
         consB
         ["dUid", "bSonUid", "dDate", "name", "fUid", "fName", "fSurn", "fSurn2", "mUid", "mName", "mSurn", "mNick"])
  m <-
    runIO $
    getMarriageFromStringList
      (map consM ["dUid", "dDate", "uid1", "name1", "sname11", "sname12", "uid2", "name2", "sname21", "sname22"])
  d <-
    runIO $
    getDeathFromStringList
      (map consD ["dUid", "persUid", "date", "name", "surname", "year", "cause", "age", "job", "test", "muni", "desc"])
  describe "Person data type" $ do
    it "[getPersonsInBirthDoc] Should return three Person in birth document" $
      -- Son data
     do
      (pUid $ son b) `shouldBe` (fromString $ consB "bSonUid")
      (pDocUid $ son b) `shouldBe` (returnM . fromString $ consB "dUid")
      (pDate $ son b) `shouldBe` Just (ModifiedJulianDay 55479)
      (pName $ son b) `shouldBe` consB "name"
      (pSurnames $ son b) `shouldBe` consB "fSurn" ++ " " ++ consB "mSurn"
      (pNickName $ son b) `shouldBe` ""
      (pRole $ son b) `shouldBe` Son
      -- Father data
      (pUid $ father b) `shouldBe` (fromString $ consB "fUid")
      (pDocUid $ father b) `shouldBe` (returnM . fromString $ consB "dUid")
      (pDate $ father b) `shouldBe` Just (ModifiedJulianDay 55479)
      (pName $ father b) `shouldBe` consB "fName"
      (pSurnames $ father b) `shouldBe` consB "fSurn"
      (pNickName $ father b) `shouldBe` ""
      (pRole $ father b) `shouldBe` Father
      -- Mother data
      (pUid $ mother b) `shouldBe` (fromString $ consB "mUid")
      (pDocUid $ mother b) `shouldBe` (returnM . fromString $ consB "dUid")
      (pDate $ mother b) `shouldBe` Just (ModifiedJulianDay 55479)
      (pName $ mother b) `shouldBe` consB "mName"
      (pSurnames $ mother b) `shouldBe` consB "mSurn"
      (pNickName $ mother b) `shouldBe` consB "mNick"
      (pRole $ mother b) `shouldBe` Mother
    it "[getPersonsInMarriageDoc] Should return two Person in marriage document" $
      -- Bridegroom 1
     do
      (pUid $ bdgr1 m) `shouldBe` (fromString $ consM "uid1")
      (pDocUid $ bdgr1 m) `shouldBe` (returnM . fromString $ consM "dUid")
      (pDate $ bdgr1 m) `shouldBe` Just (ModifiedJulianDay 55479)
      (pName $ bdgr1 m) `shouldBe` consM "name1"
      (pSurnames $ bdgr1 m) `shouldBe` consM "sname11"
      (pNickName $ bdgr1 m) `shouldBe` consM ""
      (pRole $ bdgr1 m) `shouldBe` Bridegroom
      -- Bridegroom 2
      (pUid $ bdgr2 m) `shouldBe` (fromString $ consM "uid2")
      (pDocUid $ bdgr2 m) `shouldBe` (returnM . fromString $ consM "dUid")
      (pDate $ bdgr2 m) `shouldBe` Just (ModifiedJulianDay 55479)
      (pName $ bdgr2 m) `shouldBe` consM "name2"
      (pSurnames $ bdgr2 m) `shouldBe` consM "sname21"
      (pNickName $ bdgr2 m) `shouldBe` consM ""
      (pRole $ bdgr2 m) `shouldBe` Bridegroom
    it "[getPersonsInDeathDoc] Should return a Person in death document" $
      -- Deceased
     do
      (pUid $ death d) `shouldBe` (fromString $ consD "persUid")
      (pDocUid $ death d) `shouldBe` (returnM . fromString $ consD "dUid")
      (pDate $ death d) `shouldBe` Just (ModifiedJulianDay 55479)
      (pName $ death d) `shouldBe` consD "name"
      (pSurnames $ death d) `shouldBe` consD "surname"
      (pNickName $ death d) `shouldBe` consD ""
      (pRole $ death d) `shouldBe` Deceased
    it "[getPersons] Should return a list of people in given docs" $ do
      let persons = getPersons [returnM b] [returnM m] [returnM d]
      length persons `shouldBe` 6
    it "[getDeathsFromPersonList] Transforms a list of people in a list of docs" $ do
      getDeathsFromPersonList
        [ (Person
             (fromString $ "4187f6e1-97b5-4cd9-bd48-1a397f78cc55")
             (returnM . fromString $ "4187f6e4-97b5-4cd9-bd48-1a397f78cc55")
             (date "12/12/2000")
             "Antonio Armando"
             "Carrascosa Bedulia"
             "El Mozo"
             Deceased)
        , Person
            (fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc55")
            (returnM . fromString $ "4187f6e6-97b5-4cd9-bd48-1a397f78cc55")
            (date "12/12/2018")
            "Arm"
            "Ca:r,r. B."
            ""
            Bridegroom
        , Person
            (fromString $ "4187f6e3-97b5-4cd9-bd48-1a397f78cc55")
            (returnM . fromString $ "4187f6e7-97b5-4cd9-bd48-1a397f78cc55")
            (date "12/12/2018")
            "Pepo"
            "Pintilla"
            ""
            Son
        ] `shouldBe`
        [ (Death
             (returnM . fromString $ "4187f6e4-97b5-4cd9-bd48-1a397f78cc55")
             (fromString "4187f6e1-97b5-4cd9-bd48-1a397f78cc55")
             (date "12/12/2000")
             "Antonio Armando"
             "Carrascosa Bedulia"
             (Just 2000)
             ""
             Nothing
             ""
             ""
             ""
             "")
        ]
  where
    returnM :: Maybe a -> a
    returnM (Just x) = x
    son = head . getPersonsInBirthDoc . returnM
    father = head . tail . getPersonsInBirthDoc . returnM
    mother = last . getPersonsInBirthDoc . returnM
    bdgr1 = head . getPersonsInMarriageDoc . returnM
    bdgr2 = last . getPersonsInMarriageDoc . returnM
    death = head . getPersonsInDeathDoc . returnM
