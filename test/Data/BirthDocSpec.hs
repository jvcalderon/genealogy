module Data.BirthDocSpec where

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
  | x == "bSonUid" = "7d3830b2-2eb2-11e9-9659-d663bd873d93"
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

spec = do
  wrongBirth <- runIO $ getBirthFromStringList ["x", "y"]
  b <-
    runIO $
    getBirthFromStringList
      (map consB ["dUid", "dDate", "bSonUid", "name", "fUid", "fName", "fSurn", "fSurn2", "mUid", "mName", "mSurn", "mNick"])
  describe "Birth doc data type" $ do
    it "[getBirthFromStringList] Should return Nothing if user doesn`t provide valid values" $ do
      wrongBirth `shouldBe` Nothing
    it "[getBirthFromStringList] Should return a valid Birth if user provides valid values" $ do
      (bUid $ lift b) `shouldBe` (lift . fromString $ consB "dUid")
      (bDate $ lift b) `shouldBe` Just (ModifiedJulianDay 55479)
      (bSonUid $ lift b) `shouldBe` (fromString $ consB "bSonUid")
      (bName $ lift b) `shouldBe` (consB "name")
      (bFatherUid $ lift b) `shouldBe` (fromString $ consB "fUid")
      (bFatherName $ lift b) `shouldBe` (consB "fName")
      (bFatherSurname1 $ lift b) `shouldBe` (consB "fSurn")
      (bFatherSurname2 $ lift b) `shouldBe` (consB "fSurn2")
      (bMotherUid $ lift b) `shouldBe` Nothing
      (bMotherName $ lift b) `shouldBe` (consB "mName")
      (bMotherSurname $ lift b) `shouldBe` (consB "mSurn")
      (bMotherNickname $ lift b) `shouldBe` (consB "mNick")
  where
    lift :: Maybe a -> a
    lift (Just x) = x
