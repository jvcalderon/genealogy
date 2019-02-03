module ParserSpec where

import Control.Exception (evaluate)
import Parser
import Test.Hspec
import Test.QuickCheck

spec = do
  wrongBirth <- runIO (getBirth ["x", "y"])
  describe "Genealogy docs parser" $ do
    it "[getBirth] Should return Nothing if user doesn`t provide valid values" $ do wrongBirth `shouldBe` Nothing
