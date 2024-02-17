{-# LANGUAGE OverloadedStrings #-}
module EconomancyEntites where
import Data.Map ( Map )
import JSONEntities
import Data.Aeson(fromJSON)

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
    deriving (Show, Eq)

getCardFromJSON :: JSONCard -> Card
getCardFromJSON jsonCard =
  Card (cardName jsonCard) (cardUses jsonCard) 0 0 0 0

data Player = Player
  { coins :: Int,
    buys :: Int,
    cards :: [Card]
  }
  deriving (Show, Eq)

getPlayerFromJSON :: JSONPlayer -> Player
getPlayerFromJSON jsonPlayer =
  Player (playerCoins jsonPlayer) (playerBuys jsonPlayer) [getCardFromJSON c | c <- playerCards jsonPlayer]

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

-- getPhaseFromJSON :: JSONPhase -> Phase
-- getPhaseFromJSON jsonPhase =
--   case JSONEntities.phaseName jsonPhase of
--     "investing" -> InvestingOrBuy "investing"
--     "buy" -> InvestingOrBuy "buy"
--     "attacking" -> case phaseAttacker_card jsonPhase of
--       Just a -> Attacking "attacking" (phaseAttacker jsonPhase) ((Data.Aeson.fromJSON a) :: Int)




data State = State
  { day :: Int,
    phase :: Phase,
    shop :: Map String Int,
    players :: [Player],
    player :: Int
  }
  deriving (Show)
