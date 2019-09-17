module Data.MarriageDocSpec where

import Control.Exception (evaluate)
import Data
import Data.Time
import Data.UUID
import Parser
import Test.Hspec
import Test.QuickCheck

consM :: String -> String
consM x
  | x == "dUid" = "4187f6e4-97b5-4cd9-bd48-1a397f78cc55"
  | x == "dDate" = "10/10/2010"
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
  wrongMarriage <- runIO $ getMarriageFromStringList ["a", "b"]
  m <-
    runIO $
    getMarriageFromStringList
      (map consM ["dUid", "dDate", "uid1", "name1", "sname11", "sname12", "uid2", "name2", "sname21", "sname22"])
  describe "Marriage data type" $ do
    it "[getMarriageFromStringList] Should return Nothing if user doesn`t provide valid values" $ do
      wrongMarriage `shouldBe` Nothing
    it "[getMarriageFromStringList] Should return a valid Marriage if user provides valid values" $ do
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
