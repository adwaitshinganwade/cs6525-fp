module Controller where
import Model (Circle (..), Vector (xComponent, yComponent), Game, Player (playerCircle))
import Data.Set (Set, fromList, toList)


checkCircleIntersection :: Circle -> Circle -> Bool
checkCircleIntersection aCircle anotherCircle =
    let 
        radiusOne = size aCircle        
        radiusTwo = size anotherCircle
    in
        (distance <= radiusOne + radiusTwo)
    where
        centerOne = location aCircle
        centerTwo = location anotherCircle
        distance = sqrt $ (xComponent centerOne - xComponent centerTwo)**2 + (yComponent centerOne - yComponent centerTwo)**2

-- Returns a set containing dead players
getPlayersCollidingWithAnotherPlayer :: Set Player -> Set Player
getPlayersCollidingWithAnotherPlayer currentPlayers =
    let 
        listOfAllPlayers = toList currentPlayers
    in
        fromList [p | p <- listOfAllPlayers, q <- listOfAllPlayers, p /= q, checkCircleIntersection (playerCircle p) (playerCircle q)]
    

-- updateGameState :: Game -- -> Game
-- updateGameState currentState =
--     let 
--         currentPlayers = players currentState
--         currentPowerUps = powerups currentState
--     in
--         -- Detect player-player collisions and populate the list of dead players

--         -- For each alive player, detect player-powerup collision and grow the player
--         -- and add the powerup to to the list of eaten powerups

--         -- Update the locations of all players based on their velocity vectors

--         -- Check if there are a minimum number of powerups and generate more if needed

--         -- Generate the next game state
    
