module UidSpec where

import Data
import Data.List       (sortBy)
import Data.Ord        (comparing)
import Data.UUID
import Test.Hspec
import Test.QuickCheck
import Uid

spec = do
  person <-
    runIO $
    assign
      [ (Person
           (fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc55")
           (returnM . fromString $ "4187f6e4-97b5-4cd9-bd48-1a397f78cc55")
           (date "01/01/1900")
           "Armando"
           "Carrascosa Bedulia"
           "El Mozo"
           Father)
      ]
      (Person
         Nothing
         (returnM . fromString $ "4187f6e4-97b5-4cd9-bd48-1a397f78cc56")
         (date "01/01/1890")
         "A."
         "Carrascosa B."
         "El Mozo"
         Bridegroom)
  personNoAssign <-
    runIO $
    assign
      [ Person
          (Just $ returnM . fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc55")
          (returnM . fromString $ "afabb4e3-7094-4131-adb3-7e5ed40859bb")
          (date "01/01/1920")
          "Arm"
          "Ca:r,r. B."
          ""
          Father
      ]
      (Person
         (Just $ returnM . fromString $ "d3d4e574-1d62-495e-bc12-5b9abd3556f0")
         (returnM . fromString $ "beef9f7c-16bb-4e11-a2ec-493a9f555a7e")
         (date "01/01/1900")
         "Antonio Armando"
         "Carrascosa Bedulia"
         "El Mozo"
         Son)
  personList <-
    runIO $
    setUids
      -- Marriage document data
      [ (Person
           Nothing
           (returnM . fromString $ "b0edccbc-7161-4eae-812f-aa4b37911daf")
           (date "01/01/1890")
           "1 Arm."
           "Carrascosa B."
           "El Mozo"
           Bridegroom)
      , (Person
           Nothing
           (returnM . fromString $ "ea9ce932-236c-42f2-87d0-9951e9a1ca40")
           (date "01/01/1890")
           "2 Mari. Luisa"
           "Zamorano Sal."
           "La Moza"
           Bridegroom)
      -- Birth document data
      , (Person
           (fromString $ "0493f00c-b786-4b18-b2b1-5430c4e0b7ef")
           (returnM . fromString $ "3ef5d425-f30d-4b41-aaa0-3c33a697b9d5")
           (date "01/01/1900")
           "3 Armando"
           "Carrascosa Zamorano"
           ""
           Son)
      , (Person
           Nothing
           (returnM . fromString $ "3ef5d425-f30d-4b41-aaa0-3c33a697b9d5")
           (date "01/01/1900")
           "4 Armando"
           "Carrascosa Bedulia"
           "El Mozo"
           Father)
      , (Person
           Nothing
           (returnM . fromString $ "3ef5d425-f30d-4b41-aaa0-3c33a697b9d5")
           (date "01/01/1900")
           "5 María Luisa"
           "Zamorano Salamanca"
           "La Chula"
           Mother)
      -- Death documents data
      , (Person
           Nothing
           (returnM . fromString $ "4187f6e4-97b5-4cd9-bd48-1a397f78cc57")
           (date "01/01/1900")
           "6 A."
           "Carrascosa B."
           ""
           Deceased)
      , (Person
           Nothing
           (returnM . fromString $ "4187f6e4-97b5-4cd9-bd48-1a397f78cc58")
           (date "01/01/2010")
           "7 Arm."
           "Carrascosa"
           "El Mozo"
           Deceased)
      ]
  sortedPList <- return $ sortBy (comparing pName) personList
  describe "Identifies people as unique persons by using certain criteria" $ do
    it "[assign] should assign an uid to single person if matches with another one" $ do
      (pName person) `shouldBe` "A."
      (pUid person) `shouldBe` (fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc55")
    it
      "[assign] should not assign an uid to single person if uid is already assigned, even if it matches with another one" $ do
      (pUid personNoAssign) `shouldBe` (fromString $ "d3d4e574-1d62-495e-bc12-5b9abd3556f0")
    it "[setUids] should assign an uid to everyone and assign the same uid to people figured out as the same one" $
      -- Fist row remains to a father with the same name as his son. He must receive a new uid and it must be different
     do
      length sortedPList `shouldBe` 7
      (pName $ sortedPList !! 0) `shouldBe` "1 Arm."
      (pUid $ sortedPList !! 0) `shouldNotBe` Nothing
      (pUid $ sortedPList !! 0) `shouldNotBe` (pUid $ sortedPList !! 2)
      -- Second row remains to a person without any match in whole list. It must receive a new uid
      (pName $ sortedPList !! 1) `shouldBe` "2 Mari. Luisa"
      (pUid $ sortedPList !! 1) `shouldNotBe` Nothing
      -- Third row already has uid. It must be keeped
      (pName $ sortedPList !! 2) `shouldBe` "3 Armando"
      (pUid $ sortedPList !! 2) `shouldBe` fromString "0493f00c-b786-4b18-b2b1-5430c4e0b7ef"
      -- Fourth row must has the same uid as the first one. It remains to the same person
      (pName $ sortedPList !! 3) `shouldBe` "4 Armando"
      (pUid $ sortedPList !! 3) `shouldBe` (pUid $ sortedPList !! 0)
      -- Fifth row must has the same uid as the second one. It remains to the same person
      (pName $ sortedPList !! 4) `shouldBe` "5 María Luisa"
      (pUid $ sortedPList !! 4) `shouldBe` (pUid $ sortedPList !! 1)
      -- Sixth row can't be resolved. It has more than one match; could be the father or the son. Both have the same name
      (pName $ sortedPList !! 5) `shouldBe` "6 A."
      (pUid $ sortedPList !! 5) `shouldBe` Nothing
      -- Seventh row must be a new person because of time lapse
      (pName $ sortedPList !! 6) `shouldBe` "7 Arm."
      (pUid $ sortedPList !! 6) `shouldNotBe` (pUid $ sortedPList !! 0)
      (pUid $ sortedPList !! 6) `shouldNotBe` (pUid $ sortedPList !! 2)
      (pUid $ sortedPList !! 6) `shouldNotBe` Nothing
  where
    returnM :: Maybe a -> a
    returnM (Just x) = x
