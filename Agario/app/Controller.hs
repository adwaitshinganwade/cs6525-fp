module Controller where
import Model (Circle (..), Vector (xComponent, yComponent, Vector2D), Game (thePlayer), Player (playerCircle, Player, playerSpeed), Powerup (..), regularPowerupSize, regularPowerupGrowthPotential)
import Data.Set (Set, fromList, toList, empty, insert)
import Data.List (delete)
import System.Random (Random(randomRs), RandomGen, StdGen)
import GraphicsUtils (windowHeight, windowWidth)
import qualified Data.Set as Set
import Graphics.Gloss (rgbaOfColor, yellow)

maxPlayerSize :: Float
maxPlayerSize = 200.0

minPlayerSpeed :: Float
minPlayerSpeed = 1.0

minPowerupCount :: Int
minPowerupCount = 20

enumerate = zip [0..]

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




-- Returns the first powerup among all alive powerups that a player can eat
getEatenPowerup :: Player -> [Powerup] -> Maybe Powerup
getEatenPowerup aPlayer [] = Nothing
getEatenPowerup aPlayer (p:ps) =
    if checkCircleIntersection (playerCircle aPlayer) (powerupCircle p)
        then Just p
    else
        getEatenPowerup aPlayer ps

-- Ensures that there are at least Controller.minPowerupCount powerups in the world. 
-- Generates random powerups as needed. 
ensureMinimumPowerupCount :: Set Powerup -> Int -> StdGen -> (Set Powerup, Int)
ensureMinimumPowerupCount alivePowerups nextId randomNumberGenerator =
    let 
        neededRefill = minPowerupCount - Set.size alivePowerups
        in
        if neededRefill > 0
            then
                let
                randomXs = take neededRefill $ randomRs (0, windowWidth - 1) randomNumberGenerator
                randomYs = take neededRefill $ randomRs (0, windowHeight - 1) randomNumberGenerator
                refilledPowerups = [
                    RegularPowerup{
                        powerupId = nextId + i, 
                        powerupCircle = Model.Circle{
                            location = Vector2D (fromIntegral $ randomXs !! i) (fromIntegral $ randomYs !! i), 
                            -- TODO (low priority): random colour for each powerup
                            colour = rgbaOfColor yellow, 
                            size = regularPowerupSize}, 
                        growthPotential = regularPowerupGrowthPotential 
                    }
                    | i <- [0..(neededRefill-1)]]
                    in
                    (Set.union (fromList refilledPowerups)  alivePowerups, nextId + (minPowerupCount - neededRefill))
                    
    else
        (alivePowerups, nextId)





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

