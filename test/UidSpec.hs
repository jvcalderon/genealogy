module UidSpec where

import Data
import Data.UUID
import Test.Hspec
import Test.QuickCheck
import Uid

spec = do
  deathList <-
    runIO $
    setUuidToAllDeaths
      [ (Person
           Nothing
           (returnM . fromString $ "4187f6e4-97b5-4cd9-bd48-1a397f78cc55")
           (date "2000-12-12")
           "Armando"
           "Carrascosa Bedulia"
           "El Mozo"
           Deceased)
      , (Person
           Nothing
           (returnM . fromString $ "4187f6e5-97b5-4cd9-bd48-1a397f78cc55")
           (date "2000-12-10")
           "Samuel"
           "Berono Bellido"
           "El Gitanote"
           Deceased)
      ]
  describe "Identifies people as unique persons by using certain criteria" $ do
    it "[setUuidToAllDeaths] should set uid to all decesaed if UID does not exist yet" $ do
      (pUid $ deathList !! 0) `shouldNotBe` Nothing
      (pUid $ deathList !! 1) `shouldNotBe` Nothing
      (pUid $ deathList !! 0) `shouldNotBe` (pUid $ deathList !! 1)
    it "[setUuidToAllDeaths] should set uid to all decesaed if UID does not exist yet" $ do
      (pUid $ deathList !! 0) `shouldNotBe` Nothing
      (pUid $ deathList !! 1) `shouldNotBe` Nothing
      (pUid $ deathList !! 0) `shouldNotBe` (pUid $ deathList !! 1)
  where
    returnM :: Maybe a -> a
    returnM (Just x) = x
