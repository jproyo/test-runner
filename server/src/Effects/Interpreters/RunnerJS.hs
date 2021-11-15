{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Effects.Interpreters.RunnerJS where

import           Colog.Polysemy                as CP
import           Control.Lens                  as L
import qualified Data.Map.Strict               as M
import           Data.Runner
import           Effects.Error                 as PE
import           Effects.Runner
import           Effects.Storage
import           JS.Utils
import           Language.JavaScript.Inline
import           Polysemy
import           Polysemy.Reader               as PR
import           Relude

-----------------------------------------------------------------------------
                     -- Effectful Interpreter Runner JS --
-----------------------------------------------------------------------------
runRunnerJS :: forall effs a
             . ( Member (Embed IO) effs
               , Members
                   '[ AppError
                    , Log Text
                    , Storage
                    , PR.Reader (TMVar TestState)
                    ]
                   effs
               )
            => Sem (Runner ': effs) a
            -> Sem effs a
runRunnerJS = interpret $ \case
  SubmitTask testId test -> runInJS testId test


runInJS :: ( Member (Embed IO) effs
           , Members
               '[AppError , Log Text , Storage , PR.Reader (TMVar TestState)]
               effs
           )
        => TestId
        -> Test
        -> Sem effs ()
runInJS testId test = do
  currentState <- PR.ask
  CP.log $ "Submitting test with id " <> show testId <> " - Test " <> show test
  void $ embed $ newSession defaultConfig >>= \_session -> do
    _callback <- export _session (onFinished currentState testId test)
    eval @()
      _session
      [block|

          const generateDummyTest = () => {
            var delay = 7000 + Math.random() * 7000;
            var testPassed = Math.random();

            return function (callback) {
              setTimeout(function () {
                callback(testPassed);
              }, delay);
            };
          };

          const callTest = generateDummyTest();
          callTest($_callback);
      |]
  updateStatus testId test Running

onFinished :: TMVar TestState -> TestId -> Test -> F32 -> IO ()
onFinished st testId test (F32 passed) = atomically $ do
  currentState <- readTMVar st
  let newSt = newStatus passed
  let upd = M.update
        (Just . over (_Wrapped' . traversed . filteredBy (only test))
                     (tStatus .~ newSt)
        )
        testId
        currentState
  void $ swapTMVar st upd

