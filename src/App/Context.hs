module App.Context where

import           Control.Lens            hiding ( (.=) )
import           Data.Aeson
import           Data.Yaml.Config
import           Deriving.Aeson
import           Options.Applicative           as Opt
import           Relude


newtype TestRunnerConf = TestRunnerConf
  { _trcPort      :: Int
  }
  deriving Generic
  deriving FromJSON 
  via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "_trc", CamelToSnake]] TestRunnerConf

$(makeLenses ''TestRunnerConf)

configurationOption :: Opt.Parser FilePath
configurationOption = Opt.strOption
  (  Opt.long "configuration"
  <> Opt.short 'f'
  <> Opt.metavar "FilePath"
  <> Opt.help "Configuration file"
  )

appConf :: Opt.ParserInfo FilePath
appConf = info
  (configurationOption <**> helper)
  (fullDesc <> progDesc "Test Runner App" <> header
    "test-runner - Test Runner App"
  )

readFileConf :: FilePath -> IO TestRunnerConf
readFileConf file = loadYamlSettings [file] [] useEnv

