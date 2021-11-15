module Effects.Runner where

import           Data.Runner
import           Polysemy


--------------------------------------------------------------------------------
                               -- Effect Runner Model --
--------------------------------------------------------------------------------
data Runner r a where
  SubmitTask :: TestId -> Test -> Runner r ()

makeSem ''Runner
