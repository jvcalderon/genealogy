module WriterSpec where

import Data
import Data.UUID
import Test.Hspec
import Test.QuickCheck
import Writer

spec = do
  describe "Handle content and write genealogy files" $ do
    it "[docUids] Should return unique doc uids in person list" $ do
      docUids
        [ Person
            (fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc55")
            (returnM . fromString $ "4187f6e6-97b5-4cd9-bd48-1a397f78cc55")
            (date "2018-12-12")
            "Arm"
            "Ca:r,r. B."
            ""
            Bridegroom
        , Person
            (fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc56")
            (returnM . fromString $ "4187f6e6-97b5-4cd9-bd48-1a397f78cc55")
            (date "2018-12-12")
            "Arm"
            "Ca:r,r. B."
            ""
            Bridegroom
        , Person
            (fromString "4187f6e3-97b5-4cd9-bd48-1a397f78cc55")
            (returnM . fromString $ "4187f6e7-97b5-4cd9-bd48-1a397f78cc55")
            (date "2018-12-12")
            "Pepo"
            "Pintilla"
            ""
            Son
        ] `shouldBe`
        [ returnM . fromString $ "4187f6e6-97b5-4cd9-bd48-1a397f78cc55"
        , returnM . fromString $ "4187f6e7-97b5-4cd9-bd48-1a397f78cc55"
        ]
  where
    returnM :: Maybe a -> a
    returnM (Just x) = x
