{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module EconomancyEntites where

import qualified Data.Map as Map
import Data.Map (fromList, Map)
import qualified JSONEntities
import Data.Aeson ( fromJSON, Value, Value(Bool) )
import Data.Aeson.Decoding.Tokens (Number)
import Data.Aeson.Types (Value(Number))


{- 
The dayEarnings field captures the coins that a card can earn purely based on the current day. Special
earnings (such as those provided by Magic Bean Stock), are modelled separately 
-}

referenceCards :: Map String Card =
  Map.fromList [
    ("Sorcerer's Stipend", Card{name="Sorcerer's Stipend", uses=0, attack=0, defense=0, victory_points=0, cost=0, dayEarnings=[1, 0, 0]}),
    ("Board of Monopoly", Card{name="Board of Monopoly", uses=0, attack=1, defense=1, victory_points=1, cost=2, dayEarnings=[0, 0, 0]}),
    ("Incantation", Card{name="Incantation", uses=0, attack=1, defense=1, victory_points=3, cost=4, dayEarnings=[0, 0, 0]}),
    ("Worker", Card{name="Worker", uses=0, attack=1, defense=2, victory_points=0, cost=1, dayEarnings=[0, 1, 1]}),
    ("Bubble", Card{name="Bubble", uses=0, attack=9, defense=2, victory_points=0, cost=2, dayEarnings=[0, 0, 0]}),
    ("Magic Bean Stock", Card{name="Sorcerer's Stipend", uses=0, attack=1, defense=1, victory_points=0, cost=1, dayEarnings=[0, 0, 0]}),
    ("Ghost", Card{name="Ghost", uses=0, attack=3, defense=2, victory_points=0, cost=2, dayEarnings=[0, 0, 1]}),
    ("Senior Worker", Card{name="Senior Worker", uses=0, attack=2, defense=2, victory_points=0, cost=2, dayEarnings=[1, 1, 1]}),
    ("Gold Fish", Card{name="Gold Fish", uses=0, attack=1, defense=2, victory_points=0, cost=3, dayEarnings=[0, 0, 4]}),
    ("Wall of Wealth", Card{name="Wall of Wealth", uses=0, attack=1, defense=2, victory_points=0, cost=1, dayEarnings=[1, 0, 0]}),
    ("Apprentice", Card{name="Apprentice", uses=0, attack=2, defense=1, victory_points=0, cost=3, dayEarnings=[1, 1, 0]}),
    ("Thug", Card{name="Thug", uses=0, attack=4, defense=4, victory_points=0, cost=3, dayEarnings=[0, 1, 0]}),
    ("Shield of Greed", Card{name="Shield of Greed", uses=0, attack=2, defense=7, victory_points=0, cost=4, dayEarnings=[0, 0, 0]}),
    ("Golem", Card{name="Golem", uses=0, attack=7, defense=7, victory_points=0, cost=5, dayEarnings=[0, 0, 0]})
  ]

type Day = Int

type Coins = Int

type Uses = Int

data Card
    = Card
    {
        name :: String,
        uses :: Uses,
        attack :: Int,
        defense :: Int,
        victory_points :: Int,
        cost :: Int,
        dayEarnings :: [Int]
    }
    deriving (Show, Eq)

getCardFromJSON :: JSONEntities.JSONCard -> Card
getCardFromJSON jsonCard =
  let nameOfCard = JSONEntities.cardName jsonCard
      thisCard = referenceCards Map.! nameOfCard
  in
  Card nameOfCard (JSONEntities.cardUses jsonCard) (attack thisCard) (defense thisCard) (victory_points thisCard) (cost thisCard) [0, 0, 0]

{-
Returns the coins a card earns on a certain day
-}
cardEarnings :: State -> Card -> Day -> Coins
cardEarnings state card day =
  let
    thisPlayer = currentPlayer state
    cardIncome :: Coins = dayEarnings (referenceCards Map.! name card) !! day
  in
    cardIncome + applyIncomeSpecialEffects state thisPlayer day card




data Player = Player
  { coins :: Coins,
    buys :: Int,
    cards :: [Card]
  }
  deriving (Show, Eq)

getPlayerFromJSON :: JSONEntities.JSONPlayer -> Player
getPlayerFromJSON jsonPlayer =
  Player (JSONEntities.playerCoins jsonPlayer) (JSONEntities.playerBuys jsonPlayer) [getCardFromJSON c | c <- JSONEntities.playerCards jsonPlayer]

currentPlayer :: State -> Player
currentPlayer state = (players :: State -> [Player]) state !! (player :: State -> Int) state

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

getPhaseFromJSON :: JSONEntities.JSONPhase -> Phase
getPhaseFromJSON jsonPhase =
  case JSONEntities.phaseName jsonPhase of
    "investing" -> InvestingOrBuy "investing"
    "buy" -> InvestingOrBuy "buy"
    "attacking" -> case JSONEntities.phaseAttacker_card jsonPhase of
      Just numberOrBool ->
        case numberOrBool of
          Number n -> let attackerCard = JSONEntities.fromJSONValue numberOrBool :: Maybe Int
                          (Just attackerId) = JSONEntities.phaseAttacker jsonPhase
            in Attacking "attacking" attackerId attackerCard
          Bool b -> let (Just attackerId) = JSONEntities.phaseAttacker jsonPhase
            in Attacking "attacking" attackerId Nothing
    "end" -> End "end" (JSONEntities.phaseWinner jsonPhase)
    -- Exit if phase is unknown
    _ -> End "end" (Just (-1))


data State = State
  { day :: Day,
    phase :: Phase,
    shop :: Map String Int,
    players :: [Player],
    player :: Int
  }
  deriving (Show)


getStateFromJSON :: Maybe JSONEntities.JSONState -> State
getStateFromJSON maybeJsonState =
  case maybeJsonState of
    (Just jsonState) ->
      let (Just shopFromJSON) = JSONEntities.fromJSONValue (JSONEntities.shop jsonState) :: Maybe (Map String Int)
      in State (JSONEntities.day jsonState)
        (getPhaseFromJSON (JSONEntities.phase jsonState))
        shopFromJSON
        [getPlayerFromJSON p | p <- JSONEntities.players jsonState]
        (case JSONEntities.player jsonState of
          Just n -> n
          Nothing -> -1)
    Nothing -> error "Could not parse state from JSON"


applyIncomeSpecialEffects :: State -> Player -> Day -> Card -> Coins
applyIncomeSpecialEffects state player day card =
  let
    playerCoins = coins player
    playerCards = cards player
    currentPhase = (phase :: State -> Phase) state
  in
  case card of
    Card "Shield of Greed" _ _ _ _ _ _ ->
      case currentPhase of
        Attacking _ _ Nothing -> 1
        _ -> 0
    Card "Sorcerer's Stipend" _ _ _ _ _ _ -> 1
    {- a coin for every 3 coins earned by cards to the left of this one -}
    Card "Magic Bean Stock" _ _ _ _ _ _ ->
      (playerCoins - 1 + sum [if name (fst c) /= "Magic Bean Stock"
        then cardEarnings state (fst c) day
        else incomeFromMagicBeanStock state day (playerCoins - 1) playerCards (snd c)
        | c <- zip playerCards [0..]])
        `div` 3
    _ -> 0

incomeFromMagicBeanStock :: State -> Day -> Coins -> [Card] -> Int -> Int
incomeFromMagicBeanStock state day coinBalance playerCards mbsIndex =
  let
    cardsBeforeThisMBS = take mbsIndex playerCards
  in
    (coinBalance + sum [ if name (fst c) /= "Magic Bean Stock" then
      cardEarnings state (fst c) day 
      else incomeFromMagicBeanStock state day coinBalance playerCards (snd c)
      | c <- zip cardsBeforeThisMBS [0..]])
    `div`
    3