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
  -- If the total attacking potential is less than that of the total 
  -- attacking potential of half the remaining players, invest all coins
  let
    allButThisPlayer = [totalAttackingPotential p | p <- players state,  p /= thisPlayer]
    strongerThanPlayer = numberOfStrongerOpponents (totalAttackingPotential thisPlayer) allButThisPlayer
  in if strongerThanPlayer >= length allButThisPlayer `div` 2
    then coins thisPlayer
    else randomInvestment
    where thisPlayer = players state !! player state

totalAttackingPotential :: Player -> Int
totalAttackingPotential player =
  sum [attack (referenceCards Map.! (name :: Card -> String) card) | card <- cards player]


numberOfStrongerOpponents :: (Foldable t, Ord a, Num b) => a -> t a -> b
numberOfStrongerOpponents strongerThan = foldl (\x y -> if y > strongerThan then x+1 else x) 0

-- Chooses the strongest untapped card
chooseAttackingCard :: State -> Int
chooseAttackingCard state =
  let playerCards = cards thisPlayer
      strongestCard = getStrongestAttackingCard playerCards 
      in strongestCard
      where thisPlayer = players state !! player state


-- Get strongest attacking card by index (maybe later balance attack and defense?)
getStrongestAttackingCard :: [Card] -> Int
getStrongestAttackingCard deck =
  let enumeratedDeck = zip [0..(length deck - 1)] deck
  in
  -- No Wall of Wealth card yet, hence based around uses == 0
  fst (foldl (\x y -> if uses (snd y) == 0 && attack (snd y) > attack (snd x) then y else x) (0, head deck) enumeratedDeck) 