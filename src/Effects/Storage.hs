module Effects.Storage where

import           Data.Runner
import           Polysemy
import           Relude


--------------------------------------------------------------------------------
                               -- Effect Runner Model --
--------------------------------------------------------------------------------
data Storage r a where
  Save   ::TestId -> TestSet -> Storage r ()
  UpdateStatus ::TestId -> Test -> TestStatus -> Storage r ()
  Get    ::TestId -> Storage r (Maybe TestSet)
  NotEmpty  :: Storage r Bool

makeSem ''Storage
