{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module JSONEntities where
import GHC.Generics
import Data.Aeson ( FromJSON, Value)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson as JSON
import GHC.Desugar ((>>>))
import Data.Char (toLower)

jsonOptions :: String -> Aeson.Options
jsonOptions fieldPrefix =
    let prefixChars = length fieldPrefix
        toLowercase(c : cs) = toLower c : cs
        toLowercase [] = []
        in Aeson.defaultOptions {JSON.fieldLabelModifier = drop prefixChars >>> toLowercase}

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
    phaseAttacker :: Int,
    phaseAttacker_card :: Maybe Value,
    winner :: Maybe String 
} deriving (Generic, Show)

instance FromJSON JSONPhase where
    parseJSON = JSON.genericParseJSON $ jsonOptions "phase"

data JSONState = JSONState {
    day :: Int,
    phase :: JSONPhase,
    shop :: Value,
    players :: [JSONPlayer],
    player :: Int
} deriving (Generic, Show)

instance FromJSON JSONState