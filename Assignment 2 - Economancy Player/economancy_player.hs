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

-- TODO - earning per day, separate function of day and card?
    
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
    deriving (Show)

data Player = Player
  { coins :: Int,
    buys :: Int,
    cards :: [Card]
  }
  deriving (Show)

data Phase
  = InvestingOrBuy {name :: String}
    | Attacking
      { name :: String,
        attacker :: Int,
        attacker_card :: Maybe Int
      }
    | End {
        name :: String,
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
