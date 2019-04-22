module ToolsSpec where

import Data
import Data.UUID
import Test.Hspec
import Test.QuickCheck
import Tools

spec = do
  describe "Provides some tools to handle common actions" $ do
    it "[getMatches] Gets persons who matches by name or surname" $ do
      (pName . head)
        (getMatches
           (Person
              Nothing
              (returnM . fromString $ "4187f6e4-97b5-4cd9-bd48-1a397f78cc55")
              (date "2000-12-12")
              "Armando"
              "Carrascosa Bedulia"
              "El Mozo"
              Deceased)
           [ Person
               Nothing
               (returnM . fromString $ "4187f6e6-97b5-4cd9-bd48-1a397f78cc55")
               (date "2018-12-12")
               "Arm"
               "C. bedulia"
               ""
               Bridegroom
           , Person
               Nothing
               (returnM . fromString $ "4187f6e7-97b5-4cd9-bd48-1a397f78cc55")
               (date "2018-12-12")
               "Pepo"
               "Pintilla"
               ""
               Son
           ]) `shouldBe`
        "Arm"
  where
    returnM :: Maybe a -> a
    returnM (Just x) = x
