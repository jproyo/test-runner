{-# LANGUAGE TypeOperators   #-}

module App.Internal where


import           App.Context
import           Colog                         as C
import           Control.Lens            hiding ( Context )
import           Data.Runner
import           Effects.Algebras        hiding ( run )
import qualified Elm.Derive
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.MakeAssets
import           Network.Wai.Middleware.Cors
import           Relude
import           Runner.Test
import           Servant                       as S

-- brittany-disable-next-binding
type TestRunnerApi =  "api" :> "runner" :> 
                     ("submit" :> ReqBody '[JSON] TestsToRun :> Post '[JSON] TestsToRunResponse
                     :<|> "new" :> ReqBody '[JSON] TestsToRun :> Post '[JSON] TestsToRunResponse
                     :<|> "status" :> Capture "testId" Text :> Get '[JSON] TestsToRunResponse)

type TestRunnerApp =  TestRunnerApi :<|> Raw

testRunnerApp :: Proxy TestRunnerApp
testRunnerApp = Proxy

options :: Options
options = Options "client"

server :: Application -> ServerT TestRunnerApp (Sem InterpreterApp)
server assets =
  (submitEndpointCurrent
    :<|> submitNewEndpoint
    :<|> getStatuses)
    :<|> Tagged assets

submitEndpointCurrent :: TestsToRun -> Sem InterpreterApp TestsToRunResponse
submitEndpointCurrent = submitTestsCurrent

submitNewEndpoint :: TestsToRun -> Sem InterpreterApp TestsToRunResponse
submitNewEndpoint = submitTestsNew

getStatuses :: Text -> Sem InterpreterApp TestsToRunResponse
getStatuses = statuses

startApp :: TestRunnerConf -> TMVar TestState -> Application -> Application
startApp _ s assets =
  serve testRunnerApp $ hoistServer testRunnerApp (runEffects s) (server assets)

addCors :: Middleware
addCors = cors $ const $ Just $ CorsResourcePolicy
  { corsOrigins        = Nothing
  , corsMethods        = [methodPost, methodGet, methodDelete, methodPut]
  , corsRequestHeaders = [hAuthorization, hContentType]
  , corsExposedHeaders = Nothing
  , corsMaxAge         = Nothing
  , corsVaryOrigin     = True
  , corsRequireOrigin  = False
  , corsIgnoreFailures = True
  }

runApp :: TestRunnerConf -> IO ()
runApp conf = do
  appMemory <- newTMVarIO emptyState
  let action = cmap fmtMessage logTextStdout
  let port   = conf ^. trcPort
  usingLoggerT action $ do
    C.log I "Starting Test Runner App ...."
    C.log I $ "Listening on port " <> show port
  assets <- serveAssets options
  run port . addCors $ startApp conf appMemory assets

Elm.Derive.deriveBoth Elm.Derive.defaultOptions  ''TestsToRun
Elm.Derive.deriveBoth (Elm.Derive.defaultOptionsDropLower 4) ''TestToRun
Elm.Derive.deriveBoth (Elm.Derive.defaultOptionsDropLower 5) ''TestsToRunResponse
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''TestId
Elm.Derive.deriveBoth (Elm.Derive.defaultOptionsDropLower 2) ''Test
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''GeneralStatus
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''TestStatus
