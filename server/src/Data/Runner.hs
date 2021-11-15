module Data.Runner where

import           Control.Lens
import qualified Data.Map.Strict               as M
import           Deriving.Aeson
import           Relude

data TestStatus = NotStartedYet
                | Running
                | Passed
                | Failed
  deriving (Generic, Show)

data GeneralStatus = NotStarted
                   | Submitted
                   | InProgress
                   | Finished
  deriving (Generic, Show)

newtype TestsToRun = TestsToRun [TestToRun]
  deriving (Generic, Show)

data TestToRun = TestToRun
  { _ttrDescription :: Text
  , _ttrRun         :: Text
  }
  deriving (Generic, Show)

data TestsToRunResponse = TestsToRunResponse
  { _ttrrGeneralStatus :: GeneralStatus
  , _ttrrTestSetId     :: TestId
  , _ttrrResults       :: [Test]
  }
  deriving (Generic, Show)

newtype TestId = TestId Text
  deriving (Generic, Show)
  deriving newtype (Ord, Eq, IsString)

data Test = Test
  { _tId          :: TestId
  , _tStatus      :: TestStatus
  , _tDescription :: Text
  }
  deriving (Generic, Show)

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
