module Data.Legislator where


import           Control.Lens
import           Data.Aeson
import           Data.Char
import           Data.Time.Calendar
import           Deriving.Aeson
import           Relude

data ToLower
instance StringModifier ToLower where
  getStringModifier ""       = ""
  getStringModifier (c : xs) = toLower c : xs

data Gender = M | F
  deriving (Generic, Show, Eq, Ord)
  deriving (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields] Gender

newtype Bio = Bio { _bGender :: Gender }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "_b" ,CamelToSnake]] Bio

data TermType = Sen | Rep
  deriving (Generic, Show, Eq, Ord)
  deriving (ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[ToLower]] TermType

instance FromJSON TermType where
  parseJSON (String o) = case o of
    "sen" -> pure Sen
    "rep" -> pure Rep
    e     -> fail $ "No String that matches representative type " <> toString e
  parseJSON _ = fail "No String that matches representative type"

data Term = Term
  { _tType  :: TermType
  , _tStart :: Day
  , _tEnd   :: Day
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via CustomJSON
    '[OmitNothingFields , FieldLabelModifier '[StripPrefix "_t" , CamelToSnake]]
    Term

data Legislator = Legislator
  { _lBio   :: Bio
  , _lTerms :: [Term]
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via CustomJSON
    '[OmitNothingFields , FieldLabelModifier '[StripPrefix "_l" , CamelToSnake]]
    Legislator


makeLenses ''Legislator
makeLenses ''Term

isFemale :: Legislator -> Bool
isFemale = (== F) . _bGender . _lBio

isRep :: Term -> Bool
isRep = (== Rep) . _tType

toYear :: Day -> Integer
toYear = fstf . toGregorian where fstf (y, _, _) = y

years :: Term -> [Integer]
years t = [(t ^. tStart . to toYear) .. (t ^. tEnd . to toYear)]

