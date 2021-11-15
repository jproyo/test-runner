module Runner.Test
  ( statuses
  , submitTestsNew
  , submitTestsCurrent
  ) where

import           Colog.Polysemy                as CP
import           Control.Lens
import           Data.Runner
import           Effects.Algebras              as EA
import           Relude

data SubmitState = New | Current
newtype Submit (s :: SubmitState) = Submit TestsToRun

statuses :: Members '[Log Text , AppError , Storage] eff
         => Text
         -> Sem eff TestsToRunResponse
statuses testId = do
  result <- EA.get $ TestId testId
  CP.log $ "Getting results for testId " <> show testId <> " - results " <> show result
  maybe
    (EA.throw $ NO_TESTS_SET_FOUND ("No test found for id " <> show testId))
    (pure . toTestsToRunResponse (TestId testId))
    result

submitTestsNew :: Members
                    '[Log Text , AppError , Storage , Runner , EA.Async , IdGen]
                    eff
               => TestsToRun
               -> Sem eff TestsToRunResponse
submitTestsNew = submitTestsNew' . Submit

submitTestsCurrent :: Members
                        '[ Log Text
                         , AppError
                         , Storage
                         , Runner
                         , EA.Async
                         , IdGen
                         ]
                        eff
                   => TestsToRun
                   -> Sem eff TestsToRunResponse
submitTestsCurrent = submitTestsCurrent' . Submit

submitTestsNew' :: Members
                     '[ Log Text
                      , AppError
                      , Storage
                      , Runner
                      , EA.Async
                      , IdGen
                      ]
                     eff
                => Submit 'New
                -> Sem eff TestsToRunResponse
submitTestsNew' (Submit tests) = do
  submitted <- submitNew tests
  CP.log $ "Submitting new batch tests " <> show submitted
  save (submitted ^. ttrrTestSetId) (submitted ^. ttrrResults . to TestSet)
  return submitted

submitTestsCurrent' :: Members
                         '[ Log Text
                          , AppError
                          , Storage
                          , Runner
                          , EA.Async
                          , IdGen
                          ]
                         eff
                    => Submit 'Current
                    -> Sem eff TestsToRunResponse
submitTestsCurrent' (Submit tests) =
  ifM notEmpty
    ( EA.throw
      $ ALREADY_SUBMITTED "Tests Already submitted. Wait for finishing"
    )
    $ submitTestsNew' (Submit tests)


submitNew :: Members
               '[Log Text , AppError , Storage , Runner , EA.Async , IdGen]
               eff
          => TestsToRun
          -> Sem eff TestsToRunResponse
submitNew (TestsToRun tests) = do
  testId <- genNew
  toRun  <- mapM
    (\t -> Test <$> genNew <*> pure NotStartedYet <*> pure (t ^. ttrDescription)
    )
    tests
  let tasks = map (submitTask testId) toRun
  void $ EA.async $ EA.sequenceConcurrently tasks
  return $ toTestsToRunResponse testId (TestSet toRun)

