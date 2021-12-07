module Legislator.LegislatorSpec
  ( spec
  ) where

import qualified Data.Map.Strict as M
import           Data.Aeson
import           LegislatorHistory
import           Relude
import           Test.Hspec


spec :: Spec
spec = do
  describe "Run Test over some mocked Legislator List" $ do
    it "Expected 4 years in ascending order" $ do
      ls <- eitherDecodeFileStrict @[Legislator] "./data/legislators-historical.json"
      case ls of
        Left e -> expectationFailure $ show e
        Right legislators -> do 
          let h = histogram legislators
          output h
          M.null h `shouldBe` False

