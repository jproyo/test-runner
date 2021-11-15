module Effects.Interpreters.MemStorage where

import           Colog.Polysemy                as CP
import           Control.Lens                  as L
import qualified Data.Map.Strict               as M
import           Data.Runner
import           Effects.Error
import           Effects.Storage
import           Polysemy
import           Polysemy.Reader               as PR
import           Relude

-----------------------------------------------------------------------------
                     -- Effectful Interpreter Memory Storage --
-----------------------------------------------------------------------------
runMemStorage :: forall effs a
               . ( Member (Embed IO) effs
                 , Members
                     '[AppError , Log Text , PR.Reader (TMVar TestState)]
                     effs
                 )
              => Sem (Storage ': effs) a
              -> Sem effs a
runMemStorage = interpret $ \case
  Save id' tests               -> save' id' tests
  UpdateStatus id' test status -> updateStatus' id' test status
  Get id'                      -> get' id'
  NotEmpty -> notEmpty'

notEmpty' :: ( Member (Embed IO) effs
         , Members '[AppError , Log Text , PR.Reader (TMVar TestState)] effs
         )
      => Sem effs Bool
notEmpty' = fmap (not . M.null) . embed . atomically . readTMVar =<< PR.ask

save' :: ( Member (Embed IO) effs
         , Members '[AppError , Log Text , PR.Reader (TMVar TestState)] effs
         )
      => TestId
      -> TestSet
      -> Sem effs ()
save' id' tests = do
  st <- PR.ask
  embed $ atomically $ do
    currentState <- readTMVar st
    let updatedState = M.insert id' tests currentState
    void $ swapTMVar st updatedState

updateStatus' :: ( Member (Embed IO) effs
                 , Members
                     '[AppError , Log Text , PR.Reader (TMVar TestState)]
                     effs
                 )
              => TestId
              -> Test
              -> TestStatus
              -> Sem effs ()
updateStatus' id' test status = do
  st <- PR.ask
  embed $ atomically $ do
    currentState <- readTMVar st
    let upd = M.update
          (Just . over (_Wrapped' . traversed . filteredBy (only test))
                       (tStatus .~ status)
          )
          id'
          currentState
    void $ swapTMVar st upd



get' :: ( Member (Embed IO) effs
        , Members '[AppError , Log Text , PR.Reader (TMVar TestState)] effs
        )
     => TestId
     -> Sem effs (Maybe TestSet)
get' id' = do
  st     <- PR.ask
  embed $ atomically $ do
    currentState <- readTMVar st
    return $ M.lookup id' currentState
  