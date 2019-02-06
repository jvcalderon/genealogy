module Data
  ( BirthDoc(Birth, bUid, bDate, bName, bFatherUid, bFatherName,
         bFatherSurname1, bFatherSurname2, bMotherUid, bMotherName,
         bMotherSurname, bMotherNickname)
  , MarriageDoc(Marriage, mUid, mDate, mUid1, mName1, mSurname11,
            mSurname12, mUid2, mName2, mSurname21, mSurname22)
  , DeathDoc(Death, dUid, dPersonUid, dDate, dName, dSurname, dYear,
         dCause, dAge, dJob, dTestament, dMunicipality, dDescription)
  , date
  ) where

import Data.BirthDoc
import Data.Date
import Data.DeathDoc
import Data.MarriageDoc
