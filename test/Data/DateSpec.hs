module Data.DateSpec where

import Control.Exception (evaluate)
import Data
import Data.Time
import Test.Hspec
import Test.QuickCheck

spec = do
  describe "Genealogy data types" $ do
    it "[date] Should get a Day if given string is a valid date" $ do
      date "30/11/2019" `shouldBe` Just (ModifiedJulianDay 58817)
      date "02/01/2019" `shouldBe` Just (ModifiedJulianDay 58485)
      date "31/11/2019" `shouldBe` Nothing
      date "31/11/2019" `shouldBe` Nothing
    it "[dateStr] Gets a day as string" $ do
      dateStr (ModifiedJulianDay 58817) `shouldBe` "30/11/2019"
      dateStr (ModifiedJulianDay 58485) `shouldBe` "02/01/2019"
