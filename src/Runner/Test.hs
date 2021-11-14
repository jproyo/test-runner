{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Runner.Test
  ( submitTests
  , statuses
  ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Lens
import           Control.Monad.Reader
import           Data.Either.Extra
import           Data.Error
import qualified Data.Map.Strict               as M
import           Data.Runner
import           Data.UUID
import           Data.UUID.V4
import           JS.Utils
import           Language.JavaScript.Inline
import           Relude


statuses :: (MonadIO m, MonadReader (TMVar TestsState) m)
         => m (Either AppError TestsToRunResponse)
statuses = do
  currentState <- ask
  st           <- liftIO $ atomically $ readTMVar currentState
  if M.null st
    then return $ Left NO_TESTS_SUBMITTED_YET
    else return $ Right $ toTestsToRunResponse st

submitTests :: (MonadIO m, MonadReader (TMVar TestsState) m)
            => TestsToRun
            -> m (Either AppError TestsToRunResponse)
submitTests tests = do
  currentState <- ask
  submitted <- submitNew tests
  liftIO $ void $ atomically $ swapTMVar currentState submitted
  return $ Right $ toTestsToRunResponse submitted

submitNew :: (MonadIO m, MonadReader (TMVar TestsState) m)
          => TestsToRun
          -> m TestsState
submitNew (TestsToRun tests) = do
  currentState <- ask
  toRun        <- mapM
    (\t -> (,) <$> liftIO nextRandom <*> pure
      (StatusDesc (t ^. ttrDescription) NotStartedYet)
    )
    tests
  liftIO $ void $ forkIO $ forConcurrently_ toRun (runInJS currentState)
  return $ M.fromList toRun

onFinished :: TMVar TestsState -> F32 -> WrapUUID -> IO ()
onFinished st (F32 passed) (WrapUUID uuid) =
  updateState st uuid (newStatus passed)

whenStarted :: TMVar TestsState -> UUID -> IO ()
whenStarted st uuid = updateState st uuid Running

updateState :: TMVar TestsState -> UUID -> TestStatus -> IO ()
updateState st uuid status = atomically $ do
  currentState <- readTMVar st
  let updatedState = M.update (Just . set sdStatus status) uuid currentState
  void $ swapTMVar st updatedState

runInJS :: TMVar TestsState -> (UUID, StatusDesc) -> IO ()
runInJS currentState (uuid, _) = do
  void $ newSession defaultConfig >>= \_session -> do
    let _uuid = WrapUUID uuid
    _callback <- export _session (onFinished currentState)
    eval @()
      _session
      [block|

          const generateDummyTest = uuid => {
            var delay = 7000 + Math.random() * 7000;
            var testPassed = Math.random();

            return function (callback) {
              setTimeout(function () {
                callback(testPassed, uuid);
              }, delay);
            };
          };

          const callTest = generateDummyTest($_uuid);
          callTest($_callback);
      |]
  whenStarted currentState uuid


