module Effects.Interpreters.IdGenUUID where

import           Data.Runner
import           Data.UUID.V4
import           Effects.IdGen
import           Polysemy
import           Relude

-----------------------------------------------------------------------------
                     -- Effectful Interpreter UUID Gen --
-----------------------------------------------------------------------------
runUUIDGen :: forall effs a
            . (Member (Embed IO) effs)
           => Sem (IdGen ': effs) a
           -> Sem effs a
runUUIDGen = interpret $ \case
  GenNew -> embed $ TestId <$> nextRandom
