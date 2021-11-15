{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module Effects.Error where

import           Control.Monad.Error.Class
import           Data.Aeson
import           Deriving.Aeson
import           Polysemy.Error                as PE
import           Relude
import           Servant

data Err = Err ErrType Int Text
  deriving Show

data ErrType
  = AlreadySubmitted
  | NoTestFoundError
  | NoTestSetFoundError
  | InternalError
  deriving (Generic, Show)
  deriving ToJSON
  via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] ErrType

class MonadError ServerError m => ServantErr m e where
  toServantError :: e -> m a

instance ToJSON Err where
  toJSON (Err t code err) =
    object ["error_code" .= toJSON t, "error_code" .= code, "message" .= err]

instance MonadError ServerError m => ServantErr m Err where
  toServantError a@(Err _ code _) = throwError $ ServerError
    { errHTTPCode     = code
    , errReasonPhrase = ""
    , errBody         = encode a
    , errHeaders      = []
    }

pattern ALREADY_SUBMITTED :: Text -> Err
pattern ALREADY_SUBMITTED msg = Err AlreadySubmitted 409 msg

pattern INTERNAL_ERROR :: Text -> Err
pattern INTERNAL_ERROR msg = Err InternalError 500 msg

pattern NO_TESTS_SUBMITTED_YET :: Err
pattern NO_TESTS_SUBMITTED_YET =
  Err NoTestFoundError 404 "No Tests has been submitted yet"

pattern NO_TESTS_SET_FOUND :: Text -> Err
pattern NO_TESTS_SET_FOUND msg = Err NoTestSetFoundError 404 msg

type AppError = PE.Error Err
