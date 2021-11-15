module JS.Utils where

import           Data.Aeson
import           Data.UUID                     as U
import           Language.JavaScript.Inline
import           Relude

newtype WrapUUID = WrapUUID UUID
  deriving (Show, Generic)
  deriving newtype (ToJSON, FromJSON)
  deriving (ToJS, FromJS) via (Aeson WrapUUID)

newtype F32 = F32 Float
  deriving (Show, Generic)
  deriving newtype (ToJSON, FromJSON)
  deriving (ToJS, FromJS) via (Aeson F32)
