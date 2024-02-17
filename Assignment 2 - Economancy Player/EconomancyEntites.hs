{-# LANGUAGE OverloadedStrings #-}
module EconomancyEntites where
import Data.Map ( Map )
import GHC.Generics
import qualified Data.Aeson as JSON;
import Data.Aeson (FromJSON(..), ToJSON(..), (.:)) 
import qualified Data.ByteString.Char8 as B
import Data.ByteString(ByteString(..))

data Card
    = Card
    {
        name :: String,
        uses :: Int,
        attack :: Int,
        defense :: Int,
        victory_points :: Int,
        cost :: Int
    }
    deriving (Show, Eq, Generic)

instance FromJSON Card where
  parseJSON = JSON.withObject "Card" $ \o -> do
    cardNameString <- o .: "name"
    cardUsesString <- o .: "uses"
    let cardName :: String = read cardNameString
    let cardUses :: Int = read cardUsesString
    pure Card{name=cardName, uses=cardUses, attack=0, defense=0, victory_points=0, cost=0}


parseCard :: ByteString -> IO Card
parseCard bs = do
  case JSON.decodeStrict bs of 
    Nothing -> error "Nothing"
    Just a -> return a

readAndParse :: IO ()
readAndParse = do
  bs <- B.getLine
  parsedCard <- (parseCard bs)
  putStrLn (name parsedCard)
     


data Player = Player
  { coins :: Int,
    buys :: Int,
    cards :: [Card]
  }
  deriving (Show, Eq)

data Phase
  = InvestingOrBuy {phaseName :: String}
    | Attacking
      { phaseName :: String,
        attacker :: Int,
        attacker_card :: Maybe Int
      }
    | End {
        phaseName :: String,
        winner :: Maybe Int}
    deriving (Show)

data State = State
  { day :: Int,
    phase :: Phase,
    shop :: Map String Int,
    players :: [Player],
    player :: Int
  }
  deriving (Show)