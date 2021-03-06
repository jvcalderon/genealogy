module Writer where

import Data
import Data.Function (on)
import Data.List
import Data.Time
import Data.UUID

docUids :: [Person] -> [UUID]
docUids = nub . map pDocUid

returnUid :: Maybe UUID -> String
returnUid (Just x) = toString x
returnUid Nothing  = ""

returnDate :: Maybe Day -> String
returnDate (Just x) = dateStr x
returnDate Nothing  = ""

getDeathFileContent :: [Person] -> [String]
getDeathFileContent = map deathFactory . filter (\x -> pRole x == Deceased)
  where
    deathFactory :: Person -> String
    deathFactory x =
      intercalate
        ";"
        [ toString . pDocUid $ x
        , returnUid . pUid $ x
        , returnDate . pDate $ x
        , pName x
        , pSurnames x
        , ""
        , ""
        , ""
        , ""
        , ""
        , ""
        , ""
        ]

getBirthsFileContent :: [Person] -> [String]
getBirthsFileContent xs =
  map birthFactory $
  filter (\xs -> length xs == 3) $ groupBy (on (==) pDocUid) (filter (\x -> pRole x `elem` [Son, Mother, Father]) xs)
  where
    birthFactory :: [Person] -> String
    birthFactory xs =
      intercalate
        ";"
        [ toString . pDocUid . son $ xs
        , returnUid . pUid . son $ xs
        , returnDate . pDate . father $ xs
        , pName . son $ xs
        , returnUid . pUid . father $ xs
        , pName . father $ xs
        , pSurnames . father $ xs
        , ""
        , returnUid . pUid . mother $ xs
        , pName . mother $ xs
        , pSurnames . mother $ xs
        , pNickName . mother $ xs
        ]
    getByRole :: Role -> [Person] -> Person
    getByRole role = head . filter (\x -> pRole x == role)
    father :: [Person] -> Person
    father = getByRole Father
    mother :: [Person] -> Person
    mother = getByRole Mother
    son :: [Person] -> Person
    son = getByRole Son

getMarriagesFileContent :: [Person] -> [String]
getMarriagesFileContent xs = map marriageFactory $ groupBy (on (==) pDocUid) (filter (\x -> pRole x == Bridegroom) xs)
  where
    marriageFactory :: [Person] -> String
    marriageFactory xs =
      intercalate
        ";"
        [ toString . pDocUid . head $ xs
        , returnDate . pDate . head $ xs
        , returnUid . pUid . head $ xs
        , pName . head $ xs
        , pSurnames . head $ xs
        , ""
        , returnUid . pUid . last $ xs
        , pName . last $ xs
        , ""
        ]
