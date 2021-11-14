module Data.Runner where

import           Control.Lens
import           Data.Aeson
import qualified Data.Map.Strict               as M
import           Data.UUID
import           Deriving.Aeson
import           Relude


data TestStatus = NotStartedYet
                | Running
                | Passed
                | Failed
  deriving (Generic, Show)
  deriving ToJSON via CustomJSON
    '[ OmitNothingFields
     , FieldLabelModifier '[CamelToSnake]
     ]
    TestStatus

type Function = Text

newtype TestsToRun = TestsToRun [TestToRun]
  deriving (Generic, Show)
  deriving newtype FromJSON

data TestToRun = TestToRun
  { _ttrDescription :: Text
  , _ttrRun         :: Function
  }
  deriving (Generic, Show)
  deriving FromJSON via CustomJSON
    '[ OmitNothingFields
     , FieldLabelModifier '[StripPrefix "_ttr" , CamelToSnake]
     ]
    TestToRun

newtype TestsToRunResponse = TestsToRunResponse [TestToRunResp]
  deriving (Generic, Show)
  deriving newtype ToJSON

data StatusDesc = StatusDesc
  { _sdDescription :: Text
  , _sdStatus      :: TestStatus
  }

data TestToRunResp = TestToRunResp
  { _ttrrId          :: UUID
  , _ttrrDescription :: Text
  , _ttrrStatus      :: TestStatus
  }
  deriving (Generic, Show)
  deriving ToJSON via CustomJSON
    '[ OmitNothingFields
     , FieldLabelModifier '[StripPrefix "_ttrr" , CamelToSnake]
     ]
    TestToRunResp


type TestsState = M.Map UUID StatusDesc

makeLenses ''TestToRunResp
makeLenses ''TestsToRunResponse
makeLenses ''TestToRun
makePrisms ''TestStatus
makeLenses ''StatusDesc

emptyState :: TestsState
emptyState = M.empty

toTestsToRunResponse :: TestsState -> TestsToRunResponse
toTestsToRunResponse =
  TestsToRunResponse . fmap (\(uuid, sd) -> TestToRunResp uuid (sd^.sdDescription) (sd^.sdStatus)) . M.toList

newStatus :: Float -> TestStatus
newStatus s | s > 0.5   = Passed
            | otherwise = Failed
