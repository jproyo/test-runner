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

newtype TestsToRun = TestsToRun [TestToRun]
  deriving (Generic, Show)
  deriving newtype FromJSON

data TestToRun = TestToRun
  { _ttrDescription :: Text
  , _ttrRun         :: Text
  }
  deriving (Generic, Show)
  deriving FromJSON via CustomJSON
    '[ OmitNothingFields
     , FieldLabelModifier '[StripPrefix "_ttr" , CamelToSnake]
     ]
    TestToRun

data TestsToRunResponse = TestsToRunResponse
  { _ttrrGeneralStatus :: GeneralStatus
  , _ttrrTestSetId     :: TestId
  , _ttrrResults       :: [Test]
  }
  deriving (Generic, Show)
  deriving ToJSON via CustomJSON
    '[ OmitNothingFields
     , FieldLabelModifier '[StripPrefix "_ttrr" , CamelToSnake]
     ]
    TestsToRunResponse

newtype TestId = TestId UUID
  deriving (Generic, Show)
  deriving newtype (FromJSON, ToJSON, Ord, Eq)

data Test = Test
  { _tId          :: TestId
  , _tStatus      :: TestStatus
  , _tDescription :: Text
  }
  deriving (Generic, Show)
  deriving ToJSON via CustomJSON
    '[OmitNothingFields , FieldLabelModifier '[StripPrefix "_t" , CamelToSnake]]
    Test

instance Eq Test where
  (==) t1 t2 = _tId t1 == _tId t2

newtype TestSet = TestSet [Test]
  deriving (Generic, Show)

instance Wrapped TestSet

type TestState = M.Map TestId TestSet

makeLenses ''TestsToRunResponse
makeLenses ''TestToRun
makePrisms ''TestStatus
makeLenses ''Test
makeLenses ''TestSet
makeLenses ''TestId

inProgress :: TestStatus -> Bool
inProgress NotStartedYet = False
inProgress _             = True

hasFinished :: TestStatus -> Bool
hasFinished Passed = True
hasFinished Failed = True
hasFinished _      = False

emptyState :: TestState
emptyState = M.empty

toTestsToRunResponse :: TestId -> TestSet -> TestsToRunResponse
toTestsToRunResponse testId (TestSet st) =
  let generalStatus | all (hasFinished . view tStatus) st = Finished
                    | any (inProgress . view tStatus) st  = InProgress
                    | otherwise                           = Submitted
  in  TestsToRunResponse generalStatus testId st

newStatus :: Float -> TestStatus
newStatus s | s > 0.5   = Passed
            | otherwise = Failed
