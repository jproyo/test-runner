module Control.Program where

import           Control.Lens
import           Data.Legislator
import qualified Data.Map.Strict               as M
import           Relude

-- TODO: Refine Types
type Year = Integer
type Histogram = M.Map Year Integer

histogram :: [Legislator] -> Histogram
histogram = foldl' buildHisto M.empty . filter isFemale

buildHisto :: Histogram -> Legislator -> Histogram
buildHisto h l =
  let years' = l ^.. lTerms . folded . filteredBy (tType . only Rep) . to years . folded
   in foldl' (flip (M.alter (maybe (Just 1) (Just . (+) 1)))) h years'
