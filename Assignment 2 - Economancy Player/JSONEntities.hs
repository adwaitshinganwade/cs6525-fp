{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module JSONEntities where
import GHC.Generics
import Data.Aeson ( FromJSON, Value)
import qualified Data.Aeson as JSON
import GHC.Desugar ((>>>))
import Data.Char (toLower)
import Data.Aeson.Types (parseMaybe)

{-|
Some of the types below use prefixed field names to
avoid clashes with their non-JSON counterparts defined
in EconomancyEntities. This function removes the specified
prefix from each field of a givem type and lowercases the
first letter of the result. This ensure an exact match
between the JSON fields and the type fields used for
reflection
-}
jsonOptions :: String -> JSON.Options
jsonOptions fieldPrefix =
    let prefixChars = length fieldPrefix
        toLowercase(c : cs) = toLower c : cs
        toLowercase [] = []
        in JSON.defaultOptions {JSON.fieldLabelModifier = drop prefixChars >>> toLowercase}

data JSONCard = JSONCard {
    cardName :: String,
    cardUses :: Int
} deriving (Generic, Show)

instance FromJSON JSONCard where
    parseJSON = JSON.genericParseJSON $ jsonOptions "card"

data JSONPlayer = JSONPlayer {
    playerCoins :: Int,
    playerBuys :: Int,
    playerCards :: [JSONCard]
} deriving (Generic, Show)

instance FromJSON JSONPlayer where
    parseJSON = JSON.genericParseJSON $ jsonOptions "player"

data JSONPhase = JSONPhase {
    phaseName :: String,
    phaseAttacker :: Maybe Int,
    phaseAttacker_card :: Maybe Value,
    phaseWinner :: Maybe Int 
} deriving (Generic, Show)

instance FromJSON JSONPhase where
    parseJSON = JSON.genericParseJSON $ jsonOptions "phase"

data JSONState = JSONState {
    day :: Int,
    phase :: JSONPhase,
    shop :: Value,
    players :: [JSONPlayer],
    player :: Maybe Int
} deriving (Generic, Show)

instance FromJSON JSONState where 
    parseJSON = JSON.genericParseJSON $ jsonOptions ""

{-|
Transforms a plain JSON value into the required value
-}
fromJSONValue :: FromJSON a => Value -> Maybe a
fromJSONValue = parseMaybe JSON.parseJSON