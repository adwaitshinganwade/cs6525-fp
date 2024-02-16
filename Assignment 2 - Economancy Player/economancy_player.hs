{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Data.Map (Map, fromList)
import qualified Data.Map as Map

referenceCards :: Map String Card =
  Map.fromList [
    ("Sorcerer's Stipend", Card{name="Sorcerer's Stipend", uses=0, attack=0, defense=0, victory_points=0, cost=0}),
    ("Board of Monopoly", Card{name="Board of Monopoly", uses=0, attack=1, defense=1, victory_points=1, cost=2}),
    ("Incantation", Card{name="Incantation", uses=0, attack=1, defense=1, victory_points=3, cost=4}),
    ("Worker", Card{name="Worker", uses=0, attack=1, defense=2, victory_points=0, cost=1}),
    ("Bubble", Card{name="Bubble", uses=0, attack=9, defense=2, victory_points=0, cost=2}),
    ("Magic Bean Stock", Card{name="Sorcerer's Stipend", uses=0, attack=0, defense=0, victory_points=0, cost=0}), -- Cannot attack
    ("Ghost", Card{name="Ghost", uses=0, attack=3, defense=2, victory_points=0, cost=2}),
    ("Senior Worker", Card{name="Senior Worker", uses=0, attack=2, defense=2, victory_points=0, cost=2}),
    ("Gold Fish", Card{name="Gold Fish", uses=0, attack=1, defense=2, victory_points=0, cost=0})
  ]


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


-- Gets the amount of coins the player wishes to invest, which will be
-- between 0 and the maximum coins the player has. The second paramter
-- is a random number in [0, coins player]
getInvestment :: State -> Int -> Int
getInvestment state randomInvestment =
  -- If this player has a higher attacking potential than the total defense
  -- potential of half the remaining players, then find the smallest amount
  -- of coins that can let this player attack. If there aren't enough coins
  -- invest all coins. Else, invest some random amount. 
  let
    totalDefenseOfOpponents = [totalPotential defense p | p <- players state,  p /= thisPlayer]
    strongerThanPlayer = numberOfStrongerOpponents (totalPotential attack thisPlayer) totalDefenseOfOpponents
    coinsWithOpponents = [coins p | p <- players state, p /= thisPlayer]
    coinsWithPlayer = coins thisPlayer
    maximumCoinsWithOpponent = maximum coinsWithOpponents
    hasMoreCoinsThanPlayer = numberOfStrongerOpponents (coins thisPlayer) coinsWithOpponents
  in if strongerThanPlayer >= length totalDefenseOfOpponents `div` 2 && hasMoreCoinsThanPlayer >= length totalDefenseOfOpponents `div` 2 then
      let
        smartInvestment = if min maximumCoinsWithOpponent coinsWithPlayer < coinsWithPlayer 
          then maximumCoinsWithOpponent + 1
          else coinsWithPlayer
      in smartInvestment
    else randomInvestment
    where thisPlayer = players state !! player state

totalPotential :: (Card -> Int) -> Player -> Int
totalPotential potentialOf player =
  sum [potentialOf (referenceCards Map.! (name :: Card -> String) card) | card <- cards player]


numberOfStrongerOpponents :: (Foldable t, Ord a, Num b) => a -> t a -> b
numberOfStrongerOpponents strongerThan = foldl (\x y -> if y > strongerThan then x+1 else x) 0

-- Chooses the strongest untapped card based on attacking or defending capacity. We assume
-- that this choice will be made only if the player has a choice (there is least one
-- untapped card with the player). If attacking is true, the attacking capacity is considered.
-- Else, the defending capacity is considered.
nextAttackMove :: State -> Bool -> Int
nextAttackMove state attacking =
  let playerCards = cards thisPlayer
      strongestCard = if attacking 
        then getStrongestCard attack playerCards 
        else getStrongestCard defense playerCards 
      in strongestCard
      where thisPlayer = players state !! player state


getStrongestCard :: (Card -> Int) -> [Card] -> Int
getStrongestCard strength deck =
  let enumeratedDeck = zip [0..(length deck - 1)] deck
  in
  -- No Wall of Wealth card yet, hence based around uses == 0
  fst (foldl (\x y -> if uses (snd y) == 0 && strength (snd y) > strength (snd x) then y else x) (0, head deck) enumeratedDeck) 