module ParserSpec where

import Control.Exception (evaluate)
import Data
import Data.Time
import Data.UUID
import Parser
import Test.Hspec
import Test.QuickCheck

cons :: String -> String
cons x
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

spec = do
  wrongBirth <- runIO $ getBirth ["x", "y"]
  rightBirth <-
    runIO $
    getBirth (map cons ["dUid", "dDate", "name", "fUid", "fName", "fSurn", "fSurn2", "mUid", "mName", "mSurn", "mNick"])
  describe "Genealogy docs parser" $ do
    it "[getBirth] Should return Nothing if user doesn`t provide valid values" $ do wrongBirth `shouldBe` Nothing
    it "[getBirth] Should return a valid Birth if user provides valid values" $ do
      (bUid $ liftMaybe rightBirth) `shouldBe` (liftMaybe . fromString $ cons "dUid")
      (bDate $ liftMaybe rightBirth) `shouldBe` Just (ModifiedJulianDay 55479)
      (bName $ liftMaybe rightBirth) `shouldBe` (cons "name")
      (bFatherUid $ liftMaybe rightBirth) `shouldBe` (fromString $ cons "fUid")
      (bFatherName $ liftMaybe rightBirth) `shouldBe` (cons "fName")
      (bFatherSurname1 $ liftMaybe rightBirth) `shouldBe` (cons "fSurn")
      (bFatherSurname2 $ liftMaybe rightBirth) `shouldBe` (cons "fSurn2")
      (bMotherUid $ liftMaybe rightBirth) `shouldBe` Nothing
      (bMotherName $ liftMaybe rightBirth) `shouldBe` (cons "mName")
      (bMotherSurname $ liftMaybe rightBirth) `shouldBe` (cons "mSurn")
      (bMotherNickname $ liftMaybe rightBirth) `shouldBe` (cons "mNick")
  where
    liftMaybe :: Maybe a -> a
    liftMaybe (Just x) = x
