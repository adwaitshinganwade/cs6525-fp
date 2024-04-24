module Controller where
import Model
import Data.Set (Set, fromList, toList, empty, insert, difference)
import Data.List (delete)
import qualified Data.Map
import System.Random (Random(randomRs), RandomGen, StdGen, mkStdGen)
import GraphicsUtils (windowHeight, windowWidth)
import Graphics.Gloss (rgbaOfColor, yellow, green, Picture, pictures)
import Data.Maybe (fromJust)



render :: Game -> Picture
render game =
    pictures $ map drawPlayer (toList $ players game) ++ map drawPowerup (toList $ powerups game)

initAgario :: StdGen -> Game
initAgario rnGenerator = Agario {
    players = fromList [
       generateNewPlayerAtRandomLocation rnGenerator empty empty 0
    ],
    deadPlayers = empty,
    eatenPowerups = empty,
        -- TODO : Manage the next powerup index
        powerups = fst $ ensureMinimumPowerupCount empty 0 rnGenerator,
    thePlayer = 0,
    nextPlayerId = 1,
    nextPowerupId = 20,
    rnGen = mkStdGen 2432
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



updateGameState ::Float -> Game -> Game
updateGameState elapsedSeconds currentState =
    let
        currentPlayers = players currentState
        currentPowerUps = powerups currentState
        currentNextPowerupId = nextPowerupId currentState
        thisRnGen = rnGen currentState

        -- Detect player-player collisions and populate the list of dead players
        nsDeadPlayers = getPlayersCollidingWithAnotherPlayer currentPlayers

        nsNonDeadPlayers = toList $ difference currentPlayers nsDeadPlayers

        -- For each alive player, detect player-powerup collision
        playerPowerupPairs = getEatenPowerups nsNonDeadPlayers (toList currentPowerUps)

        -- Convert player-powerup pairs to a map
        playersThatWillGrow = Data.Map.fromList (toList playerPowerupPairs)

        --  Generate list of player that will be alive in the next step (and grow player that eat a powerup)
        nsAlivePlayers = [
                if Data.Map.member plyr playersThatWillGrow
                then playerEatsPowerup plyr (fromJust $ Data.Map.lookup plyr playersThatWillGrow)
                else plyr
                | plyr <- nsNonDeadPlayers
            ]

        -- Add dead powerups to to the set of eaten powerups
        nsDeadPowerups = fromList [snd p | p <- toList playerPowerupPairs]

        -- Update the locations of all alive players based on their velocity vectors
        nsMovedAlivePlayers = fromList [advancePlayer plyr | plyr <- nsAlivePlayers]

        -- Check if there are a minimum number of powerups and generate more if needed
        (nsPowerups, nsNextPowerupId) = ensureMinimumPowerupCount (difference currentPowerUps nsDeadPowerups) currentNextPowerupId (mkStdGen $ nextPowerupId currentState)

    in
        currentState {
            players = nsMovedAlivePlayers,
            deadPlayers = nsDeadPlayers,
            powerups = nsPowerups,
            nextPowerupId = nsNextPowerupId,
            eatenPowerups = nsDeadPowerups
        }

