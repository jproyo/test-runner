module App.RunnerSpec
  ( spec
  ) where

import Control.Lens
import           Effects.Algebras              as EA
import           Relude
import           Runner
import           Test.Hspec
import           Test.QuickCheck               as TQ

instance Arbitrary TestToRun where
  arbitrary = TestToRun <$> fmap toText (arbitrary @[Char]) <*> fmap
    toText
    (arbitrary @[Char])

spec :: Spec
spec = do
  describe "Run Test Runner Program Effects" $ do
    it "Run submit new test batch" $ property $ \(tests :: [TestToRun]) -> do
      st     <- liftIO $ newTMVarIO emptyState
      result <- liftIO $ runWithTest st (submitTestsNew $ TestsToRun tests)
      result `shouldSatisfy` isRight
    it "Run submit same test batch and return error" $ property $ \(tests :: [TestToRun]) -> do
      st     <- liftIO $ newTMVarIO emptyState
      result <- liftIO $ runWithTest st (submitTestsNew $ TestsToRun tests)
      result `shouldSatisfy` isRight
      result' <- liftIO $ runWithTest st (submitTestsCurrent $ TestsToRun tests)
      result' `shouldSatisfy` isLeft
    it "Get Status on Current Batch" $ property $ \(tests :: [TestToRun]) -> do
      st     <- liftIO $ newTMVarIO emptyState
      result <- liftIO $ runWithTest st (submitTestsNew $ TestsToRun tests)
      case result of 
        Right r -> do 
          result' <- liftIO $ runWithTest st (statuses (r^.ttrrTestSetId))
          result' `shouldSatisfy` isRight
        Left _ -> expectationFailure "IMPOSIBLE"


runWithTest :: TMVar TestState -> Sem InterpreterApp a -> IO (Either Err a)
runWithTest st =
  liftIO
    . runM
    . asyncToIO
    . runLogAction @IO mempty
    . runError
    . EA.runReader st
    . runUUIDGen
    . runMemStorage
    . runRunnerTest


runRunnerTest :: Sem (Runner ': effs) a -> Sem effs a
runRunnerTest = interpret $ \case
  SubmitTask _ _ -> return ()

absurd' :: MonadFail m => b -> m a
absurd' = const (fail "IMPOSIBLE")
