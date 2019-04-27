module MatcherSpec where

import Data
import Data.UUID
import Matcher
import Test.Hspec
import Test.QuickCheck

spec = do
  describe "Match people who can be candidates to be the same person" $ do
    it "[getMatches] Gets persons who matches by name or surname" $ do
      (pName . head)
        (matchesByNameOrSurname
           (Person
              (Just $ returnM . fromString $ "4187f6e1-97b5-4cd9-bd48-1a397f78cc55")
              (returnM . fromString $ "4187f6e4-97b5-4cd9-bd48-1a397f78cc55")
              (date "2000-12-12")
              "Antonio Armando"
              "Carrascosa Bedulia"
              "El Mozo"
              Deceased)
           [ Person
               (Just $ returnM . fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc55")
               (returnM . fromString $ "4187f6e6-97b5-4cd9-bd48-1a397f78cc55")
               (date "2018-12-12")
               "Arm"
               "Ca:r,r. B."
               ""
               Bridegroom
           , Person
               (Just $ returnM . fromString $ "4187f6e3-97b5-4cd9-bd48-1a397f78cc55")
               (returnM . fromString $ "4187f6e7-97b5-4cd9-bd48-1a397f78cc55")
               (date "2018-12-12")
               "Pepo"
               "Pintilla"
               ""
               Son
           ]) `shouldBe`
        "Arm"
    it
      "[justOneDie] People just can die one. Two roles 'Deceased' for same person is impossible. It must be removed from list" $ do
      length
        (justOneDie
           (Person
              (Just $ returnM . fromString $ "4187f6e1-97b5-4cd9-bd48-1a397f78cc55")
              (returnM . fromString $ "4187f6e4-97b5-4cd9-bd48-1a397f78cc55")
              (date "2000-12-12")
              "Antonio Armando"
              "Carrascosa Bedulia"
              "El Mozo"
              Deceased)
           [ Person
               (Just $ returnM . fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc55")
               (returnM . fromString $ "4187f6e6-97b6-4cd9-bd48-1a397f78cc55")
               (date "2018-12-12")
               "Arm"
               "Ca:r,r. B."
               ""
               Deceased
           ]) `shouldBe`
        0
  where
    returnM :: Maybe a -> a
    returnM (Just x) = x
