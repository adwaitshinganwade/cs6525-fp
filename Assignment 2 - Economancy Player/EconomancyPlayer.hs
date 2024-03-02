{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE DuplicateRecordFields #-}



import qualified Data.Map as Map
import Data.Map (Map, fromList, elems)
import Data.ByteString (getLine, fromStrict)
import EconomancyEntites(Card(..), Player(..), State(..), Phase(..), referenceCards, getStateFromJSON, Day, Coins, currentPlayer, cardEarnings, cardEarnings)
import Data.Time.Clock.POSIX
import qualified Data.Aeson as JSON
import qualified JSONEntities (JSONState (day))
import System.Exit (exitSuccess, ExitCode (ExitSuccess), exitWith)
import System.IO ( stdout, hFlush )
import System.Random
import Data.Foldable (maximumBy)
import Data.Ord (comparing)


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
  in if length totalDefenseOfOpponents > 2 &&
    strongerThanPlayer < length totalDefenseOfOpponents `div` 2 &&
    hasMoreCoinsThanPlayer < length totalDefenseOfOpponents `div` 2 then
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
        then getStrongestCard attack playerCards True
        else getStrongestCard defense playerCards False
      in strongestCard
      where thisPlayer = currentPlayer state


getStrongestCard :: (Card -> Int) -> [Card] -> Bool -> Int
getStrongestCard strength deck forAttack =
  {- Set uses for the Bubble card to 1 while attacking to ignore it -}
  let usableDeck = if forAttack then [if (name :: Card -> String) c /= "Bubble" then c 
      else Card{name="Bubble", uses=1, attack=9, defense=2, victory_points=0, cost=2, dayEarnings=[0, 0, 0]} | c <- deck]
      else deck
      enumeratedDeck = zip [0..(length usableDeck - 1)] usableDeck
  in
    if null usableDeck 
      then 0
      else
      fst (
        foldl (\x y -> if ((uses (snd y) == 0) || (not forAttack && name (snd y) == "Wall of Wealth" && uses (snd y) <=1)) && strength (snd y) > strength (snd x)
        then y
        else x)
        (0, head usableDeck)
        enumeratedDeck
        )

{-
Finds the card among those that the player can afford 
which gives the highest income on the next day
-}
getCardWithHighestNextDayIncome :: State -> [Card] -> Int
getCardWithHighestNextDayIncome state availableOptions =
  let
    nextDay = (day :: State -> Day) state `mod` 2
    earnings = [cardEarnings state card nextDay | card <- availableOptions]
  in snd $ maximumBy (comparing fst) (zip earnings [0..])


{-|
Randomly choose a card to purchase based on the random number randomDecisionFlag.
Choose to either pass or select a card with the highest attack, denfense, or a 
card that maximizes income on the following day.
-}
makeAPurchase :: State -> Int -> String
makeAPurchase state randomDecisionFlag =
  let availableOptions = getViableOptions inventory coinBalance
  in if coinBalance == 0 || null availableOptions
    then "Pass"
    else
      case randomDecisionFlag `mod` 7 of
        0 -> (name :: Card -> String) (availableOptions !! getStrongestCard attack availableOptions False)
        1 -> (name :: Card -> String) (availableOptions !! getStrongestCard defense availableOptions False)
        2 -> (name :: Card -> String) (availableOptions !! getCardWithHighestNextDayIncome state availableOptions)
        3 -> (name :: Card -> String) (availableOptions !! getCardWithHighestNextDayIncome state availableOptions)
        4 -> (name :: Card -> String) (availableOptions !! getStrongestCard defense availableOptions False)
        5 -> (name :: Card -> String) (availableOptions !! getStrongestCard attack availableOptions False)
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
  input <- Data.ByteString.getLine
  let gameState = Data.ByteString.fromStrict input
  randomSeed <- round <$> getPOSIXTime
  randomNumber <- randomRIO(1, randomSeed)
  let jsonState = JSON.decode gameState :: Maybe JSONEntities.JSONState
  let state = getStateFromJSON jsonState
  case phase state of
    InvestingOrBuy phaseName -> if phaseName  == "investing"
      then putStrLn (show [getInvestment state randomNumber]) >> hFlush stdout >> main
      else putStrLn (show [makeAPurchase state randomNumber]) >> hFlush stdout >> main
    Attacking phaseName attacker attacker_card -> putStrLn (show [nextAttackMove state (attacker_card == Nothing)]) >> hFlush stdout >> main
    End phaseName winner -> exitSuccess