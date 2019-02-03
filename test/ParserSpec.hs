module ParserSpec where

import Control.Exception (evaluate)
import Data
import Data.Time
import Data.UUID
import Parser
import Test.Hspec
import Test.QuickCheck

consB :: String -> String
consB x
  | x == "dUid" = "61920c27-9f55-4d69-92e0-538824bdd349"
  | x == "dDate" = "2010-10-10"
  | x == "name" = "Pedro"
  | x == "fUid" = "3e62e2a9-6d56-43ec-9047-3aec2121c3a9"
  | x == "fName" = "Manolito"
  | x == "fSurn" = "Romagueras"
  | x == "fSurn2" = ""
  | x == "mUid" = ""
  | x == "mName" = "mName"
  | x == "mSurn" = "Aranda"
  | x == "mNick" = ""
  | otherwise = ""

consM :: String -> String
consM x
  | x == "dUid" = "4187f6e4-97b5-4cd9-bd48-1a397f78cc55"
  | x == "dDate" = "2010-10-10"
  | x == "uid1" = "90921800-7c75-490a-9cbf-a4f0b22de391"
  | x == "name1" = "Romeo"
  | x == "sname11" = "Montesco"
  | x == "sname12" = ""
  | x == "uid2" = "bc62d5f8-70c4-4d52-bae4-8fb34cdb5b44"
  | x == "name2" = "Julieta"
  | x == "sname21" = "Capuleto"
  | x == "sname22" = ""
  | otherwise = ""

spec = do
  wrongBirth <- runIO $ getBirth ["x", "y"]
  b <-
    runIO $
    getBirth
      (map consB ["dUid", "dDate", "name", "fUid", "fName", "fSurn", "fSurn2", "mUid", "mName", "mSurn", "mNick"])
  wrongMarriage <- runIO $ getMarriage ["a", "b"]
  m <-
    runIO $
    getMarriage
      (map consM ["dUid", "dDate", "uid1", "name1", "sname11", "sname12", "uid2", "name2", "sname21", "sname22"])
  describe "Genealogy parser" $ do
    it "[getBirth] Should return Nothing if user doesn`t provide valid values" $ do wrongBirth `shouldBe` Nothing
    it "[getBirth] Should return a valid Birth if user provides valid values" $ do
      (bUid $ lift b) `shouldBe` (lift . fromString $ consB "dUid")
      (bDate $ lift b) `shouldBe` Just (ModifiedJulianDay 55479)
      (bName $ lift b) `shouldBe` (consB "name")
      (bFatherUid $ lift b) `shouldBe` (fromString $ consB "fUid")
      (bFatherName $ lift b) `shouldBe` (consB "fName")
      (bFatherSurname1 $ lift b) `shouldBe` (consB "fSurn")
      (bFatherSurname2 $ lift b) `shouldBe` (consB "fSurn2")
      (bMotherUid $ lift b) `shouldBe` Nothing
      (bMotherName $ lift b) `shouldBe` (consB "mName")
      (bMotherSurname $ lift b) `shouldBe` (consB "mSurn")
      (bMotherNickname $ lift b) `shouldBe` (consB "mNick")
    it "[getMarriage] Should return Nothing if user doesn`t provide valid values" $ do wrongMarriage `shouldBe` Nothing
    it "[getMarriage] Should return a valid Marriage if user provides valid values" $ do
      (mUid $ lift m) `shouldBe` (lift . fromString $ consM "dUid")
      (mDate $ lift m) `shouldBe` Just (ModifiedJulianDay 55479)
      (mUid1 $ lift m) `shouldBe` (fromString $ consM "uid1")
      (mName1 $ lift m) `shouldBe` (consM "name1")
      (mSurname11 $ lift m) `shouldBe` (consM "sname11")
      (mSurname12 $ lift m) `shouldBe` (consM "sname12")
      (mUid2 $ lift m) `shouldBe` (fromString $ consM "uid2")
      (mName2 $ lift m) `shouldBe` (consM "name2")
      (mSurname21 $ lift m) `shouldBe` (consM "sname21")
      (mSurname22 $ lift m) `shouldBe` (consM "sname22")
  where
    lift :: Maybe a -> a
    lift (Just x) = x