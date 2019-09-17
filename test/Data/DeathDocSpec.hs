module Data.DeathDocSpec where

import Control.Exception (evaluate)
import Data
import Data.Time
import Data.UUID
import Parser
import Test.Hspec
import Test.QuickCheck

consD :: String -> String
consD x
  | x == "dUid" = "61920c27-9f55-4d69-92e0-538824bdd349"
  | x == "persUid" = "7d3830b2-2eb2-11e9-9659-d663bd873d93"
  | x == "date" = "10/10/2010"
  | x == "name" = "Armando"
  | x == "surname" = "Guadarrama"
  | x == "year" = "1220"
  | x == "cause" = "X"
  | x == "age" = "121"
  | x == "job" = ""
  | x == "test" = ""
  | x == "muni" = "Chillón"
  | x == "desc" = ""
  | otherwise = ""

spec = do
  wrongDeath <- runIO $ getDeathFromStringList ["x", "y"]
  d <-
    runIO $
    getDeathFromStringList
      (map consD ["dUid", "persUid", "date", "name", "surname", "year", "cause", "age", "job", "test", "muni", "desc"])
  describe "Death doc data type" $ do
    it "[getDeathFromStringList] Should return Nothing if user doesn`t provide valid values" $ do
      wrongDeath `shouldBe` Nothing
    it "[getDeathFromStringList] Should return a valid Birth if user provides valid values" $ do
      (dUid $ lift d) `shouldBe` (lift . fromString $ consD "dUid")
      (dPersonUid $ lift d) `shouldBe` (fromString $ consD "persUid")
      (dDate $ lift d) `shouldBe` Just (ModifiedJulianDay 55479)
      (dName $ lift d) `shouldBe` (consD "name")
      (dSurname $ lift d) `shouldBe` (consD "surname")
      (dYear $ lift d) `shouldBe` Just(1220)
      (dCause $ lift d) `shouldBe` (consD "cause")
      (dAge $ lift d) `shouldBe` Just(121)
      (dJob $ lift d) `shouldBe` (consD "job")
      (dTestament $ lift d) `shouldBe` (consD "test")
      (dMunicipality $ lift d) `shouldBe` (consD "muni")
      (dDescription $ lift d) `shouldBe` (consD "desc")
  where
    lift :: Maybe a -> a
    lift (Just x) = x
