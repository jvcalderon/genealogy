module Writer where

import Data
import Data.List
import Data.UUID

docUids :: [Person] -> [UUID]
docUids = nub . map pDocUid
