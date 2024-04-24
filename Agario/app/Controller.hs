module Controller where
import Model (Circle (..), Vector (xComponent, yComponent, Vector2D), Game (..), Player (..), Powerup (..), regularPowerupSize, regularPowerupGrowthPotential, defaultXSpeed, drawPlayer, drawPowerup)
import Data.Set (Set, fromList, toList, empty, insert)
import Data.List (delete)
import System.Random (Random(randomRs), RandomGen, StdGen)
import GraphicsUtils (windowHeight, windowWidth)
import Graphics.Gloss (rgbaOfColor, yellow, green, Picture, pictures)



render :: Game -> Picture
render game =
    pictures $ map drawPlayer (toList $ players game) ++ map drawPowerup (toList $ powerups game)

initAgario :: Game
initAgario = Agario {
    players = fromList [
        Player{playerId = 1, playerName = "", playerCircle = Model.Circle{location = Vector2D 50 50, colour = (10, 10, 10, 10) , size = 15}, playerSpeed = 10.0},
        Player{playerId = 2, playerName = "", playerCircle = Model.Circle{location = Vector2D 100 100, colour = (20, 10, 10, 10), size = 30}, playerSpeed = 15.0},
        Player{playerId = 3, playerName = "", playerCircle = Model.Circle{location = Vector2D 150 138, colour = (10, 20, 10, 10), size = 40}, playerSpeed = 16.0}
    ],
    deadPlayers = empty,
    eatenPowerups = empty,
        powerups = fromList [
        RegularPowerup{powerupId = 1, powerupCircle = Model.Circle{location = Vector2D 90 85, colour = (10, 10, 20, 10), size = regularPowerupSize}, growthPotential = 2.0},
        RegularPowerup{powerupId = 2, powerupCircle = Model.Circle{location = Vector2D 652 128, colour = (10, 10, 10, 20), size = regularPowerupSize}, growthPotential = 2.0},
        RegularPowerup{powerupId = 3, powerupCircle = Model.Circle{location = Vector2D 283 571, colour = (10, 10, 20, 20), size = regularPowerupSize}, growthPotential = 2.0}
        ],
    thePlayer = 0
    }

advancePlayers :: Float -> Game -> Game
advancePlayers elapsedSeconds currentState =
    currentState {players = fromList movedPlayers}
    where
        movedPlayers = map advancePlayer $ toList (players currentState)


advancePlayer :: Player -> Player
advancePlayer currentPlayerState =
    currentPlayerState{playerCircle = c{location = Vector2D{xComponent = fromIntegral newX, yComponent = fromIntegral newY}}}
    where
        currentPlayerSpeed = playerSpeed currentPlayerState
        c = playerCircle currentPlayerState
        newX = round (xComponent (location c) +  currentPlayerSpeed) `mod` windowWidth
        newY = round (yComponent (location c) +  currentPlayerSpeed) `mod` windowHeight



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

