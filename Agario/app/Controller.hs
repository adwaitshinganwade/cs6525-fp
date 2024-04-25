module Controller where
import Model
import Data.Set (Set, fromList, toList, empty, insert, difference, member, size)
import Data.List (delete, length)
import qualified Data.Map
import System.Random (Random(randomRs), RandomGen, StdGen, mkStdGen)
import GraphicsUtils (windowHeight, windowWidth)
import Graphics.Gloss (rgbaOfColor, yellow, green, Picture, pictures)
import Data.Maybe (fromJust)
import Graphics.Gloss.Interface.IO.Game (Event (EventMotion))
import Graphics.Gloss.Interface.Pure.Game
import Data.Time.Clock.POSIX (getPOSIXTime)



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
    if any (\x -> playerId x == 0) (toList (deadPlayers currentState)) then
        currentState {players = empty, powerups = empty}
    else
        let
            currentPlayers = players currentState
            mainPlayer = toList currentPlayers !! thePlayer currentState
            currentPowerUps = powerups currentState
            currentNextPowerupId = nextPowerupId currentState
            currentNextPlayerId = nextPlayerId currentState
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

            -- Change direction of computer players randomly based on time

            -- Update the locations of all alive players based on their velocity vectors
            nsMovedAlivePlayers = fromList [advanceMainPlayer plyr | plyr <- changeDirectionOfComputerPlayers nsAlivePlayers elapsedSeconds]

            -- Check if there are a minimum number of powerups and generate more if needed
            (nsPowerups, nsNextPowerupId) = ensureMinimumPowerupCount (difference currentPowerUps nsDeadPowerups) currentNextPowerupId (mkStdGen currentNextPowerupId)

            -- Check if there are a minimum number of computer players and generate more if needed
            (nsAllPlayers, nsNextPlayerId) = createComputerPlayerIfCountBelowMin nsMovedAlivePlayers (difference currentPowerUps nsDeadPowerups) currentNextPlayerId (mkStdGen currentNextPlayerId)

        in
            currentState {
                players = nsAllPlayers,
                nextPlayerId = nsNextPlayerId,
                deadPlayers = nsDeadPlayers,
                powerups = nsPowerups,
                nextPowerupId = nsNextPowerupId,
                eatenPowerups = nsDeadPowerups
            }


advanceMainPlayer :: Player -> Player
advanceMainPlayer currentPlayerState =
    currentPlayerState{playerCircle = c{location = Vector2D{xComponent = fromIntegral newX, yComponent = fromIntegral newY}}}
    where
        currentPlayerSpeed = playerSpeed currentPlayerState
        c = playerCircle currentPlayerState
        xDir = xComponent $ dir currentPlayerState
        yDir = yComponent $ dir currentPlayerState
        newX = round (xComponent (location c) +  currentPlayerSpeed * xDir) `mod` windowWidth
        newY = round (yComponent (location c) +  currentPlayerSpeed * yDir) `mod` windowHeight

handleKeyPress :: Event -> Game -> Game
handleKeyPress (EventKey (SpecialKey pressedKey) Down _ _) currentState =
    if any (\x -> playerId x == 0) (toList (deadPlayers currentState)) then
        currentState {players = empty, powerups = empty}
    else
        let
            currentPlayers = players currentState
            currentPlayerId = thePlayer currentState
            currentPlayer = toList currentPlayers !! currentPlayerId
            movedCurrentPlayer =
                case pressedKey of
                    KeyUp -> currentPlayer {dir = Vector2D 0.0 (-1.0)}
                    KeyDown -> currentPlayer {dir = Vector2D 0.0 1.0}
                    KeyLeft -> currentPlayer {dir = Vector2D (-1.0) 0.0}
                    KeyRight -> currentPlayer {dir = Vector2D 1.0 0.0}
                    _ -> currentPlayer
            nsPlayers = movedCurrentPlayer : [p | p <- toList currentPlayers, p /= currentPlayer]
            in
            currentState {players = fromList nsPlayers}

-- Ignore other events    
handleKeyPress _ gameState = gameState


changeDirectionOfComputerPlayers :: [Player] -> Float -> [Player]
changeDirectionOfComputerPlayers allPlayers elapsedTime =
    [if playerId p /= 0 then changeDirectionOfComputerPlayer p else p | p <- allPlayers]

changeDirectionOfComputerPlayer :: Player -> Player
changeDirectionOfComputerPlayer currentPlayer =
    let 
        loc = location (playerCircle currentPlayer)
        pX = xComponent loc
        pY = yComponent loc
        changeX = round pX `mod` 50 == 0 || round pX `mod` 80 == 0
        changeY = round pY `mod` 50 == 0 || round pY `mod` 75 == 0
    in
        changeSpeedDirection currentPlayer changeX changeY
    


changeSpeedDirection :: Player -> Bool -> Bool -> Player
changeSpeedDirection aPlayer changeX changeY =
    let
        playerDir = dir aPlayer
        xSpeed = xComponent playerDir
        ySpeed = yComponent playerDir
        newXSpeed = if changeX then (-xSpeed) else xSpeed
        newYSpeed = if changeY then (-ySpeed) else ySpeed
    in
        aPlayer {dir = Vector2D newXSpeed newYSpeed}
