{-# LANGUAGE TypeOperators   #-}

module App.Internal
  ( runApp
  ) where


import           App.Context
import           Colog                         as C
import           Control.Lens            hiding ( Context )
import           Data.Runner
import           Effects.Algebras        hiding ( run )
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Relude
import           Runner.Test
import           Servant                       as S

-- brittany-disable-next-binding
type TestRunnerApp =  "test" :> "runner" :> "submit"
                     :> ReqBody '[JSON] TestsToRun
                     :> Post '[JSON] TestsToRunResponse
                     :<|> "test" :> "runner" :> "submit" :> "new"
                     :> ReqBody '[JSON] TestsToRun
                     :> Post '[JSON] TestsToRunResponse
                     :<|> "test" :> "runner" :> "status" :> Capture "testId" TestId
                     :> Get '[JSON] TestsToRunResponse

instance FromHttpApiData TestId where
  parseQueryParam = fmap TestId . parseQueryParam

testRunnerApp :: Proxy TestRunnerApp
testRunnerApp = Proxy

server :: ServerT TestRunnerApp (Sem InterpreterApp)
server = submitEndpointCurrent :<|> submitNewEndpoint :<|> getStatuses

submitEndpointCurrent :: TestsToRun -> Sem InterpreterApp TestsToRunResponse
submitEndpointCurrent = submitTestsCurrent

submitNewEndpoint :: TestsToRun -> Sem InterpreterApp TestsToRunResponse
submitNewEndpoint = submitTestsNew

getStatuses :: TestId -> Sem InterpreterApp TestsToRunResponse
getStatuses = statuses

nt :: TMVar TestState -> Sem InterpreterApp a -> Handler a
nt = runEffects

startApp :: TestRunnerConf -> TMVar TestState -> Application
startApp _ s = serve testRunnerApp $ hoistServer testRunnerApp (nt s) server

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
  run port . addCors $ startApp conf appMemory

