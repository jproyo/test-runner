module Format.Stdout where

import           Control.Program
import qualified Data.Map.Strict               as M
import           Relude

output :: Histogram -> IO ()
output =
  mapM_ (\(k, v) -> putStrLn $ show k ++ " " ++ replicate (fromIntegral v) '#') . M.assocs
