module Main where

import           Runner
import           Options.Applicative as Opt
import           Relude

main :: IO ()
main = Opt.execParser appConf >>= readFileConf >>= runApp
