{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Data.Map (Map, fromList, elems)
import qualified Data.Map as Map
import Data.ByteString (getLine)
import EconomancyEntites(Card(..), Player(..), State(..), Phase(..))
import Data.Time.Clock.POSIX

referenceCards :: Map String Card =
  Map.fromList [
    ("Sorcerer's Stipend", Card{name="Sorcerer's Stipend", uses=0, attack=0, defense=0, victory_points=0, cost=0}),
    ("Board of Monopoly", Card{name="Board of Monopoly", uses=0, attack=1, defense=1, victory_points=1, cost=2}),
    ("Incantation", Card{name="Incantation", uses=0, attack=1, defense=1, victory_points=3, cost=4}),
    ("Worker", Card{name="Worker", uses=0, attack=1, defense=2, victory_points=0, cost=1}),
    ("Bubble", Card{name="Bubble", uses=0, attack=9, defense=2, victory_points=0, cost=2}),
    ("Magic Bean Stock", Card{name="Sorcerer's Stipend", uses=0, attack=1, defense=1, victory_points=0, cost=0}), -- Cannot attack
    ("Ghost", Card{name="Ghost", uses=0, attack=3, defense=2, victory_points=0, cost=2}),
    ("Senior Worker", Card{name="Senior Worker", uses=0, attack=2, defense=2, victory_points=0, cost=2}),
    ("Gold Fish", Card{name="Gold Fish", uses=0, attack=1, defense=2, victory_points=0, cost=0})
  ]

currentPlayer :: State -> Player
currentPlayer state = players state !! player state

{-|
Gets the amount of coins the player wishes to invest, which will be between 0 and
the maximum coins the player has. The second paramter is a random number. 
If this player has a higher attacking potential than the total defense potential
of half the remaining players, then find the smallest number of coins that can
let this player attack. If there aren't enough coins, invest all coins. Else, invest 
(random number) mod (number of coins with player + 1). 
-}
getInvestment :: State -> Int -> Int
getInvestment state randomInvestment =
  -- 
  let
    totalDefenseOfOpponents = [totalPotential defense p | p <- players state,  p /= thisPlayer]
    strongerThanPlayer = numberOfStrongerOpponents (totalPotential attack thisPlayer) totalDefenseOfOpponents
    coinsWithOpponents = [coins p | p <- players state, p /= thisPlayer]
    coinsWithPlayer = coins thisPlayer
    maximumCoinsWithOpponent = maximum coinsWithOpponents
    hasMoreCoinsThanPlayer = numberOfStrongerOpponents (coins thisPlayer) coinsWithOpponents
  in if strongerThanPlayer < length totalDefenseOfOpponents `div` 2 && hasMoreCoinsThanPlayer < length totalDefenseOfOpponents `div` 2 then
      let
        smartInvestment = if min maximumCoinsWithOpponent coinsWithPlayer < coinsWithPlayer
          then maximumCoinsWithOpponent + 1
          else coinsWithPlayer
      in smartInvestment
    else randomInvestment `mod` (coinsWithPlayer + 1)
    where thisPlayer = currentPlayer state

totalPotential :: (Card -> Int) -> Player -> Int
totalPotential potentialOf player =
  sum [potentialOf (referenceCards Map.! (name :: Card -> String) card) | card <- cards player]


numberOfStrongerOpponents :: (Foldable t, Ord a, Num b) => a -> t a -> b
numberOfStrongerOpponents strongerThan = foldl (\x y -> if y > strongerThan then x+1 else x) 0

{-|
Chooses the strongest untapped card based on attacking or defending capacity. We assume
that this choice will be made only if the player has a choice (there is least one
untapped card with the player). If attacking is true, the attacking capacity is considered.
Else, the defending capacity is considered.
-}
nextAttackMove :: State -> Bool -> Int
nextAttackMove state attacking =
  let playerCards = cards thisPlayer
      strongestCard = if attacking
        then getStrongestCard attack playerCards
        else getStrongestCard defense playerCards
      in strongestCard
      where thisPlayer = currentPlayer state


getStrongestCard :: (Card -> Int) -> [Card] -> Int
getStrongestCard strength deck =
  let enumeratedDeck = zip [0..(length deck - 1)] deck
  in
  -- No Wall of Wealth card yet, hence based around uses == 0
  fst (
    foldl (\x y -> if uses (snd y) == 0 && strength (snd y) > strength (snd x)
    then y
    else x)
    (0, head deck)
    enumeratedDeck
    )

{-|
Randomly choose a card to purchase based on the random number randomDecisionFlag.
Choose to either pass or select a card with the highest attack or denfense value.
-}
makeAPurchase :: State -> Int -> String
makeAPurchase state randomDecisionFlag =
  let availableOptions = getViableOptions inventory coinBalance
  in if coinBalance == 0
    then "Pass"
    else
      case randomDecisionFlag `mod` 5 of
        0 -> (name :: Card -> String) (availableOptions !! getStrongestCard attack availableOptions)
        1 -> (name :: Card -> String) (availableOptions !! getStrongestCard defense availableOptions)
        2 -> (name :: Card -> String) (availableOptions !! getStrongestCard attack availableOptions)
        3 -> (name :: Card -> String) (availableOptions !! getStrongestCard defense availableOptions)
        _ -> "Pass"
  where thisPlayer = currentPlayer state
        inventory = shop state
        coinBalance = coins thisPlayer


getViableOptions :: Map String Int -> Int -> [Card]
getViableOptions inventory coinBalance =
  availableOptions
    where availableOptions = [c | c <- elems referenceCards, Map.member ((name :: Card -> String) c) inventory, 
            (inventory Map.! (name :: Card -> String) c) > 0, cost c <= coinBalance]

main = do
  Data.ByteString.getLine
  randomNumber <- round <$> getPOSIXTime
  -- parse input into JSON, using a placeholder for now
  let state = State{day=0,
    phase=InvestingOrBuy{phaseName="buy"},
    shop=Map.fromList [("Sorcerer's Stipend", 2),
      ("Board of Monopoly",1 ),
      ("Incantation", 3),
      ("Worker", 4),
      ("Bubble", 1),
      ("Magic Bean Stock", 5), -- Cannot attack
      ("Ghost", 1),
      ("Senior Worker", 2),
      ("Gold Fish", 0)],
    players=[Player{coins=10, buys=1, cards=[referenceCards Map.! "Ghost", referenceCards Map.! "Sorcerer's Stipend", referenceCards Map.! "Board of Monopoly"]},
    Player{coins=7, buys=2, cards=[ referenceCards Map.! "Sorcerer's Stipend", referenceCards Map.! "Magic Bean Stock"]}],
    player=0}
  case phase state of
    InvestingOrBuy phaseName -> if phaseName  == "investing"
      then putStr (show (getInvestment state randomNumber))
      else putStr (show (makeAPurchase state randomNumber))
    Attacking phaseName attacker attacker_card -> putStr (show (nextAttackMove state (player state == attacker)))
    End phaseName winner -> putStr (show winner)