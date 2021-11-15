module Effects.IdGen where

import           Data.Runner
import           Polysemy


--------------------------------------------------------------------------------
                               -- Effect IdGen Model --
--------------------------------------------------------------------------------
data IdGen r a where
  GenNew :: IdGen r TestId

makeSem ''IdGen
