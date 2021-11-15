module Effects.Algebras
  ( module Colog.Polysemy
  , module Polysemy
  , module Polysemy.Async
  , module Polysemy.Reader
  , module Polysemy.Error
  , module Effects.Runner
  , module Effects.Error
  , module Effects.Storage
  , module Effects.IdGen
  , module Effects.Interpreters.MemStorage
  , module Effects.Interpreters.RunnerJS
  , module Effects.Interpreters.IdGenUUID
  , InterpreterApp
  , runEffects
  ) where

import           Colog
import           Colog.Polysemy
import           Data.Runner
import           Effects.Error
import           Effects.IdGen
import           Effects.Interpreters.IdGenUUID
import           Effects.Interpreters.MemStorage
import           Effects.Interpreters.RunnerJS
import           Effects.Runner
import           Effects.Storage
import           Polysemy
import           Polysemy.Async
import           Polysemy.Error
import           Polysemy.Reader
import qualified Polysemy.Reader               as PR
import           Relude
import           Servant

type InterpreterApp
  = '[ Runner
     , Storage
     , IdGen
     , PR.Reader (TMVar TestState)
     , AppError
     , Log Text
     , Async
     , Embed IO
     ]

runEffects :: TMVar TestState -> Sem InterpreterApp a -> Handler a
runEffects st =
  mapError'
    <=< ( liftIO
        . runM
        . asyncToIO
        . runLogAction @IO logTextStdout
        . runError
        . PR.runReader st
        . runUUIDGen
        . runMemStorage
        . runRunnerJS
        )

mapError' :: Either Err a -> Handler a
mapError' = either toServantError return

