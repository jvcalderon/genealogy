module Data
  ( BirthDoc(Birth, bUid, bDate, bSonUid, bName, bFatherUid,
         bFatherName, bFatherSurname1, bFatherSurname2, bMotherUid,
         bMotherName, bMotherSurname, bMotherNickname)
  , getBirthFromStringList
  , MarriageDoc(Marriage, mUid, mDate, mUid1, mName1, mSurname11,
            mSurname12, mUid2, mName2, mSurname21, mSurname22)
  , getMarriageFromStringList
  , DeathDoc(Death, dUid, dPersonUid, dDate, dName, dSurname, dYear,
         dCause, dAge, dJob, dTestament, dMunicipality, dDescription)
  , getDeathFromStringList
  , Person(Person, pUid, pDocUid, pDate, pName, pSurnames, pNickName,
       pRole)
  , Role(Son, Mother, Father, Deceased, Bridegroom)
  , getPersonsInBirthDoc
  , getPersonsInMarriageDoc
  , getPersonsInDeathDoc
  , getPersons
  , getDeathsFromPersonList
  , date
  ) where

import Data.BirthDoc
import Data.Date
import Data.DeathDoc
import Data.MarriageDoc
import Data.Person
