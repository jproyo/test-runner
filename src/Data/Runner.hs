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

data GeneralStatus = Submitted
                   | InProgress
                   | Finished
  deriving (Generic, Show)
  deriving ToJSON via CustomJSON
    '[ OmitNothingFields
     , FieldLabelModifier '[CamelToSnake]
     ]
    GeneralStatus

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

data TestsToRunResponse = TestsToRunResponse
  { _ttrrGeneralStatus :: GeneralStatus
  , _ttrrResults       :: [TestToRunResp]
  }
  deriving (Generic, Show)
  deriving ToJSON via CustomJSON
    '[ OmitNothingFields
     , FieldLabelModifier '[StripPrefix "_ttrr" , CamelToSnake]
     ]
    TestsToRunResponse

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

inProgress :: TestStatus -> Bool
inProgress NotStartedYet = False
inProgress _             = True

hasFinished :: TestStatus -> Bool
hasFinished Passed = True
hasFinished Failed = True
hasFinished _      = False

emptyState :: TestsState
emptyState = M.empty

toTestsToRunResponse :: TestsState -> TestsToRunResponse
toTestsToRunResponse st =
  let
    results =
      fmap
          (\(uuid, sd) ->
            TestToRunResp uuid (sd ^. sdDescription) (sd ^. sdStatus)
          )
        . M.toList
        $ st
    generalStatus | all (hasFinished . view ttrrStatus) results = Finished
                  | any (inProgress . view ttrrStatus) results = InProgress
                  | otherwise = Submitted
  in
    TestsToRunResponse generalStatus results

newStatus :: Float -> TestStatus
newStatus s | s > 0.5   = Passed
            | otherwise = Failed
