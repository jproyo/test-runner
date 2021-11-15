{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}


import           Data.List
import           Data.Text               hiding ( intercalate
                                                , map
                                                )
import           Relude
import           Runner
import           Servant.Elm                    ( DefineElm(DefineElm)
                                                , Proxy(Proxy)
                                                , defElmImports
                                                , defElmOptions
                                                , defaultOptions
                                                , deriveBoth
                                                , generateElmModuleWith
                                                )

main :: IO ()
main = generateElmModuleWith
  defElmOptions
  ["TestRunnerApi"]
  defElmImports
  "client/src"
  [ DefineElm (Proxy :: Proxy TestsToRun)
  , DefineElm (Proxy :: Proxy TestToRun)
  , DefineElm (Proxy :: Proxy TestsToRunResponse)
  , DefineElm (Proxy :: Proxy TestId)
  , DefineElm (Proxy :: Proxy Test)
  , DefineElm (Proxy :: Proxy GeneralStatus)
  , DefineElm (Proxy :: Proxy TestStatus)
  ]
  (Proxy :: Proxy TestRunnerApi)
