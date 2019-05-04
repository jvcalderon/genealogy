module MatcherSpec where

import Data
import Data.UUID
import Matcher
import Test.Hspec
import Test.QuickCheck

spec = do
  describe "Match people who can be candidates to be the same person" $ do
    it "[matchesByNameOrSurname] Gets persons who matches by name or surname" $ do
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
      "[justOneDie] People just can die once. Two roles 'Deceased' for same person is impossible. It must be removed from list" $ do
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
    it
      "[justOneBorn] People just can born once. Two roles 'Son' for same person is impossible. It must be removed from list" $ do
      length
        (justOneBorn
           (Person
              (Just $ returnM . fromString $ "4187f6e1-97b5-4cd9-bd48-1a397f78cc55")
              (returnM . fromString $ "4187f6e4-97b5-4cd9-bd48-1a397f78cc55")
              (date "2000-12-12")
              "Antonio Armando"
              "Carrascosa Bedulia"
              "El Mozo"
              Son)
           [ Person
               (Just $ returnM . fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc55")
               (returnM . fromString $ "4187f6e6-97b6-4cd9-bd48-1a397f78cc55")
               (date "2018-12-12")
               "Arm"
               "Ca:r,r. B."
               ""
               Son
           ]) `shouldBe`
        0
    it "[justOnceInDoc] People can't appear once in the same document" $ do
      length
        (justOnceInDoc
           (Person
              (Just $ returnM . fromString $ "4187f6e1-97b5-4cd9-bd48-1a397f78cc55")
              (returnM . fromString $ "beef9f7c-16bb-4e11-a2ec-493a9f555a7e")
              (date "2000-12-12")
              "Antonio Armando"
              "Carrascosa Bedulia"
              "El Mozo"
              Son)
           [ Person
               (Just $ returnM . fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc55")
               (returnM . fromString $ "beef9f7c-16bb-4e11-a2ec-493a9f555a7e")
               (date "2000-12-12")
               "Arm"
               "Ca:r,r. B."
               ""
               Father
           ]) `shouldBe`
        0
    it "[inCronOrder] People can't be in documents before their birth date" $ do
      length
        (inCronOrder
           (Person
              Nothing
              (returnM . fromString $ "beef9f7c-16bb-4e11-a2ec-493a9f555a7e")
              (date "1900-01-01")
              "Antonio Armando"
              "Carrascosa Bedulia"
              "El Mozo"
              Son)
           [ Person
               (Just $ returnM . fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc55")
               (returnM . fromString $ "beef9f7c-16bb-4e11-a2ec-493a9f555a7e")
               (date "1890-01-01")
               "Arm"
               "Ca:r,r. B."
               ""
               Father
           ]) `shouldBe`
        0
    it "[inCronOrder] People can't be in documents after their death" $ do
      length
        (inCronOrder
           (Person
              Nothing
              (returnM . fromString $ "beef9f7c-16bb-4e11-a2ec-493a9f555a7e")
              (date "1990-01-02")
              "Antonio Armando"
              "Carrascosa Bedulia"
              "El Mozo"
              Father)
           [ Person
               (Just $ returnM . fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc55")
               (returnM . fromString $ "beef9f7c-16bb-4e11-a2ec-493a9f555a7e")
               (date "1990-01-01")
               "Arm"
               "Ca:r,r. B."
               ""
               Deceased
           ]) `shouldBe`
        0
    it "[incompatibleRoles] A father can't be a mother" $ do
      length
        (incompatibleRoles
           (Person
              Nothing
              (returnM . fromString $ "beef9f7c-16bb-4e11-a2ec-493a9f555a7e")
              (date "1990-01-02")
              "Antonio Armando"
              "Carrascosa Bedulia"
              "El Mozo"
              Father)
           [ Person
               (Just $ returnM . fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc55")
               (returnM . fromString $ "beef9f7c-16bb-4e11-a2ec-493a9f555a7e")
               (date "1990-01-01")
               "Arm"
               "Ca:r,r. B."
               ""
               Mother
           ]) `shouldBe`
        0
    it "[maxLive] People don't live more than 110 years" $ do
      length
        (maxLive
           (Person
              Nothing
              (returnM . fromString $ "beef9f7c-16bb-4e11-a2ec-493a9f555a7e")
              (date "1900-01-01")
              "Antonio Armando"
              "Carrascosa Bedulia"
              "El Mozo"
              Son)
           [ Person
               (Just $ returnM . fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc55")
               (returnM . fromString $ "beef9f7c-16bb-4e11-a2ec-493a9f555a7e")
               (date "2010-01-01")
               "Arm"
               "Ca:r,r. B."
               ""
               Father
           ]) `shouldBe`
        0
    it "[haveUid] Discards people without UID" $ do
      length
        (haveUid
           [ Person
               (Just $ returnM . fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc55")
               (returnM . fromString $ "beef9f7c-16bb-4e11-a2ec-493a9f555a7e")
               (date "2010-01-01")
               "Arm"
               "Ca:r,r. B."
               ""
               Father
           , Person
               Nothing
               (returnM . fromString $ "beef9f7c-16bb-4e11-a2ec-493a9f555a7e")
               (date "1900-01-01")
               "Antonio Armando"
               "Carrascosa Bedulia"
               "El Mozo"
               Son
           ]) `shouldBe`
        1
    it "[getMatches] Gets persons who matches by name or surname and applies match criteria" $ do
      length
        (getMatches
           (Person
              Nothing
              (returnM . fromString $ "beef9f7c-16bb-4e11-a2ec-493a9f555a7e")
              (date "1900-01-01")
              "Antonio Armando"
              "Carrascosa Bedulia"
              "El Mozo"
              Son)
           [ Person
               (Just $ returnM . fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc55")
               (returnM . fromString $ "afabb4e3-7094-4131-adb3-7e5ed40859bb")
               (date "1920-01-01")
               "Arm"
               "Ca:r,r. B."
               ""
               Father
           ]) `shouldBe`
        1
  where
    returnM :: Maybe a -> a
    returnM (Just x) = x
