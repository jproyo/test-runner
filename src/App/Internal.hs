{-# LANGUAGE TypeOperators   #-}

module App.Internal
  ( runApp
  ) where


import           App.Context
import           Colog                         as C
import           Control.Lens            hiding ( Context )
import           Data.Error
import           Data.Runner
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
                     :<|> "test" :> "runner" :> "status"
                     :> Get '[JSON] TestsToRunResponse

type AppM = ReaderT (TMVar TestsState) Handler -- ^ Handler ~ ExceptT ServerError IO a

testRunnerApp :: Proxy TestRunnerApp
testRunnerApp = Proxy

server :: ServerT TestRunnerApp AppM
server = submitEndpointCurrent :<|> submitNewEndpoint :<|> getStatuses

submitEndpointCurrent :: TestsToRun -> AppM TestsToRunResponse
submitEndpointCurrent = mapError' <=< submitTestsCurrent

submitNewEndpoint :: TestsToRun -> AppM TestsToRunResponse
submitNewEndpoint = mapError' <=< submitTestsNew

getStatuses :: AppM TestsToRunResponse
getStatuses = statuses >>= mapError'

mapError' :: Either AppError a -> AppM a
mapError' = either toServantError return

nt :: TMVar TestsState -> AppM a -> Handler a
nt s x = runReaderT x s

startApp :: TestRunnerConf -> TMVar TestsState -> Application
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

