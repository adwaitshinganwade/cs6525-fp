module Controller where
import Model (Circle (..), Vector (xComponent, yComponent), Game (thePlayer), Player (playerCircle, Player, playerSpeed), Powerup (..))
import Data.Set (Set, fromList, toList, empty, insert)
import Data.List (delete)

maxPlayerSize :: Float
maxPlayerSize = 200.0

minPlayerSpeed :: Float
minPlayerSpeed = 1.0

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

playerEatsPowerup :: Player -> Powerup -> Player
playerEatsPowerup thePlayer thePowerup =
    thePlayer{playerCircle = (playerCircle thePlayer){size = min newSize maxPlayerSize}, playerSpeed = max (currentSize / newSize * currentSpeed) minPlayerSpeed}
    where
        currentSize = size $ playerCircle thePlayer
        growth = growthPotential thePowerup
        newSize = currentSize + growth
        currentSpeed = playerSpeed thePlayer

-- Returns consumed powerups along with the player that consumes them
getEatenPowerups :: [Player] -> [Powerup] -> Set (Player, Powerup)
getEatenPowerups [] alivePowerups = empty
getEatenPowerups alivePlayers [] = empty
getEatenPowerups (p:ps) alivePowerups =
    case getEatenPowerup p alivePowerups of
        Just f -> insert (p, f) $ getEatenPowerups ps (delete f alivePowerups)
        Nothing -> getEatenPowerups ps alivePowerups




getEatenPowerup :: Player -> [Powerup] -> Maybe Powerup
getEatenPowerup aPlayer [] = Nothing
getEatenPowerup aPlayer (p:ps) =
    if checkCircleIntersection (playerCircle aPlayer) (powerupCircle p)
        then Just p
    else
        getEatenPowerup aPlayer ps





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

