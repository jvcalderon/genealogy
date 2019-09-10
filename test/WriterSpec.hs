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
    it "[getDeathFileContent] Should convert a list of people to text filtering by deceased role" $ do
      getDeathFileContent
        [ Person
            (fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc55")
            (returnM . fromString $ "4187f6e6-97b5-4cd9-bd48-1a397f78cc55")
            (date "2018-12-12")
            "Armando"
            "Caballero"
            "El Tolillo"
            Deceased
        , Person
            (fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc56")
            (returnM . fromString $ "4187f6e6-97b5-4cd9-bd48-1a397f78cc56")
            (date "2018-12-12")
            "Fernando"
            "Borreguete"
            ""
            Deceased
        ] `shouldBe`
        [ "4187f6e6-97b5-4cd9-bd48-1a397f78cc55;4187f6e2-97b5-4cd9-bd48-1a397f78cc55;12/12/2018;Armando;Caballero;;;;;;;"
        , "4187f6e6-97b5-4cd9-bd48-1a397f78cc56;4187f6e2-97b5-4cd9-bd48-1a397f78cc56;12/12/2018;Fernando;Borreguete;;;;;;;"
        ]
    it "[getBirthsFileContent] Should convert a list of people to text filtering and grouping by birth doc" $ do
      getBirthsFileContent
        [ Person
            (fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc55")
            (returnM . fromString $ "4187f6e6-97b5-4cd9-bd48-1a397f78cc55")
            (date "2018-12-12")
            "Armando"
            "Caballero"
            "El Tolillo"
            Deceased
        , Person
            (fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc10")
            (returnM . fromString $ "4187f6e6-97b5-4cd9-bd48-1a397f78cc57")
            (date "2018-12-12")
            "Pedro"
            "Gula"
            ""
            Father
        , Person
            (fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc11")
            (returnM . fromString $ "4187f6e6-97b5-4cd9-bd48-1a397f78cc57")
            (date "2018-12-12")
            "Maria"
            "Guti"
            ""
            Mother
        , Person
            (fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc12")
            (returnM . fromString $ "4187f6e6-97b5-4cd9-bd48-1a397f78cc57")
            (date "2018-12-12")
            "Gimeno"
            "Gula Guti"
            ""
            Son
        , Person
            (fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc13")
            (returnM . fromString $ "4187f6e6-97b5-4cd9-bd48-1a397f78cc60")
            (date "2018-12-12")
            "Facundo"
            "Gracias"
            ""
            Father
        , Person
            (fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc14")
            (returnM . fromString $ "4187f6e6-97b5-4cd9-bd48-1a397f78cc60")
            (date "2018-12-12")
            "Pepa"
            "Pelucas"
            "Peritoria"
            Mother
        , Person
            (fromString $ "4187f6e2-97b5-4cd9-bd48-1a397f78cc15")
            (returnM . fromString $ "4187f6e6-97b5-4cd9-bd48-1a397f78cc60")
            (date "2018-12-12")
            "Juanillo"
            "Gracias Pelucas"
            ""
            Son
        ] `shouldBe`
        [ "4187f6e6-97b5-4cd9-bd48-1a397f78cc57;4187f6e2-97b5-4cd9-bd48-1a397f78cc12;12/12/2018;Gimeno;4187f6e2-97b5-4cd9-bd48-1a397f78cc10;Pedro;Gula;;4187f6e2-97b5-4cd9-bd48-1a397f78cc11;Maria;Guti;"
        , "4187f6e6-97b5-4cd9-bd48-1a397f78cc60;4187f6e2-97b5-4cd9-bd48-1a397f78cc15;12/12/2018;Juanillo;4187f6e2-97b5-4cd9-bd48-1a397f78cc13;Facundo;Gracias;;4187f6e2-97b5-4cd9-bd48-1a397f78cc14;Pepa;Pelucas;Peritoria"
        ]
  where
    returnM :: Maybe a -> a
    returnM (Just x) = x
