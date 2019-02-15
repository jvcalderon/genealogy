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
         ["dUid", "dDate", "bSonUid", "name", "fUid", "fName", "fSurn", "fSurn2", "mUid", "mName", "mSurn", "mNick"])
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
      (pDocUid $ son b) `shouldBe` (lift . fromString $ consB "dUid")
      (pDate $ son b) `shouldBe` Just (ModifiedJulianDay 55479)
      (pName $ son b) `shouldBe` consB "name"
      (pSurnames $ son b) `shouldBe` consB "fSurn" ++ " " ++ consB "mSurn"
      (pNickName $ son b) `shouldBe` ""
      (pRole $ son b) `shouldBe` Son
      -- Father data
      (pUid $ father b) `shouldBe` (fromString $ consB "fUid")
      (pDocUid $ father b) `shouldBe` (lift . fromString $ consB "dUid")
      (pDate $ father b) `shouldBe` Just (ModifiedJulianDay 55479)
      (pName $ father b) `shouldBe` consB "fName"
      (pSurnames $ father b) `shouldBe` consB "fSurn"
      (pNickName $ father b) `shouldBe` ""
      (pRole $ father b) `shouldBe` Father
      -- Mother data
      (pUid $ mother b) `shouldBe` (fromString $ consB "mUid")
      (pDocUid $ mother b) `shouldBe` (lift . fromString $ consB "dUid")
      (pDate $ mother b) `shouldBe` Just (ModifiedJulianDay 55479)
      (pName $ mother b) `shouldBe` consB "mName"
      (pSurnames $ mother b) `shouldBe` consB "mSurn"
      (pNickName $ mother b) `shouldBe` consB "mNick"
      (pRole $ mother b) `shouldBe` Mother
    it "[getPersonsInMarriageDoc] Should return two Person in marriage document" $
      -- Bridegroom 1
     do
      (pUid $ bdgr1 m) `shouldBe` (fromString $ consM "uid1")
      (pDocUid $ bdgr1 m) `shouldBe` (lift . fromString $ consM "dUid")
      (pDate $ bdgr1 m) `shouldBe` Just (ModifiedJulianDay 55479)
      (pName $ bdgr1 m) `shouldBe` consM "name1"
      (pSurnames $ bdgr1 m) `shouldBe` consM "sname11"
      (pNickName $ bdgr1 m) `shouldBe` consM ""
      (pRole $ bdgr1 m) `shouldBe` Bridegroom
      -- Bridegroom 2
      (pUid $ bdgr2 m) `shouldBe` (fromString $ consM "uid2")
      (pDocUid $ bdgr2 m) `shouldBe` (lift . fromString $ consM "dUid")
      (pDate $ bdgr2 m) `shouldBe` Just (ModifiedJulianDay 55479)
      (pName $ bdgr2 m) `shouldBe` consM "name2"
      (pSurnames $ bdgr2 m) `shouldBe` consM "sname21"
      (pNickName $ bdgr2 m) `shouldBe` consM ""
      (pRole $ bdgr2 m) `shouldBe` Bridegroom
    it "[getPersonsInDeathDoc] Should return a Person in death document" $
      -- Deceased
     do
      (pUid $ death d) `shouldBe` (fromString $ consD "persUid")
      (pDocUid $ death d) `shouldBe` (lift . fromString $ consD "dUid")
      (pDate $ death d) `shouldBe` Just (ModifiedJulianDay 55479)
      (pName $ death d) `shouldBe` consD "name"
      (pSurnames $ death d) `shouldBe` consD "surname"
      (pNickName $ death d) `shouldBe` consD ""
      (pRole $ death d) `shouldBe` Deceased
  where
    lift :: Maybe a -> a
    lift (Just x) = x
    son = head . getPersonsInBirthDoc . lift
    father = head . tail . getPersonsInBirthDoc . lift
    mother = last . getPersonsInBirthDoc . lift
    bdgr1 = head . getPersonsInMarriageDoc . lift
    bdgr2 = last . getPersonsInMarriageDoc . lift
    death = head . getPersonsInDeathDoc . lift
