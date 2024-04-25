module Model where
import Graphics.Gloss hiding (Vector)

import GraphicsUtils
import qualified Graphics.Gloss hiding (Vector)
import Data.Set (Set, fromList, empty, toList, insert)
import qualified Data.Set as Set
import System.Random (StdGen, Random (randomRs))
import Data.List (delete)

-- | The default x-velocity for player
defaultXSpeed :: Float
defaultXSpeed = 8.0

-- | The default y-velocity for player
defaultYSpeed  = 0

defaultPlayerSize = 10.0

-- | The radius of the circle for a regular powerup
regularPowerupSize = 10
regularPowerupGrowthPotential :: Float
regularPowerupGrowthPotential = 2.0

maxPlayerSize :: Float
maxPlayerSize = 200.0

minPlayerSpeed :: Float
minPlayerSpeed = 5.0

minPowerupCount :: Int
minPowerupCount = 10

minComputerPlayerCount :: Int
minComputerPlayerCount = 2

enumerate :: [b] -> [(Integer, b)]
enumerate = zip [0..]

{- Types and data -}
type Location = (Float, Float)

data Vector = Vector2D {
    xComponent :: Float,
    yComponent :: Float
} deriving (Show, Eq, Ord)

data Circle = Circle {
    location :: Vector,
    colour :: (Float, Float, Float, Float),
    size :: Float
} deriving (Show,  Eq, Ord)

data Player =  Player {
    playerId :: Int,
    playerCircle :: Circle,
    playerName :: String,
    playerSpeed :: Float,
    dir :: Vector
} deriving (Show, Eq, Ord)

defaultPlayer =  Player {
    playerId = 1, 
    playerName = "", 
    playerCircle = Model.Circle {
        location = Vector2D 5.0 4.0, 
        colour = rgbaOfColor red, 
        size = 4.0
        }, 
    playerSpeed = 20.0,
    dir = Vector2D 1.0 1.0
}

data Powerup = RegularPowerup {
   powerupId :: Int,
   powerupCircle :: Circle,
   growthPotential :: Float
} deriving (Show, Eq, Ord)

data Game = Agario {
    thePlayer :: Int,
    players :: Set Player,
    nextPlayerId :: Int,
    deadPlayers :: Set Player,
    powerups :: Set Powerup,
    nextPowerupId :: Int,
    eatenPowerups :: Set Powerup,
    rnGen :: StdGen
}

drawPlayer :: Player -> Picture
drawPlayer player =
    drawASolidCircle radius x y $ makeColor c1 c2 c3 c4
    where
        c = playerCircle player
        radius = size c
        currentLocation = location c
        x = xComponent currentLocation
        y = yComponent currentLocation
        (c1, c2, c3, c4) = colour c

drawPowerup :: Powerup -> Picture
drawPowerup powerup =
    case powerup of
        RegularPowerup {} -> drawAThickCircle radius 5 x y $ makeColor c1 c2 c3 c4
        where
            c = powerupCircle powerup
            radius = size c
            currentLocation = location c
            x = xComponent currentLocation
            y = yComponent currentLocation
            (c1, c2, c3, c4) = colour c

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
    thePlayer{playerCircle = (playerCircle thePlayer){size = min newSize maxPlayerSize}, playerSpeed = max (currentSpeed - currentSize / newSize * currentSpeed) minPlayerSpeed}
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
                        powerupId = nextId,
                        powerupCircle = Model.Circle{
                            location = Vector2D (fromIntegral $ head randomXs) (fromIntegral $ head randomYs),
                            -- TODO (low priority): random colour for each powerup
                            colour = rgbaOfColor yellow,
                            size = regularPowerupSize},
                        growthPotential = regularPowerupGrowthPotential
                    }]
                    in
                    (Set.union (fromList refilledPowerups)  alivePowerups, nextId + 1)

            else
                (alivePowerups, nextId)

createComputerPlayerIfCountBelowMin :: Set Player -> Set Powerup -> Int -> StdGen -> (Set Player, Int)
createComputerPlayerIfCountBelowMin alivePlayers alivePowerups nextId randomNumberGenerator =
    -- -1 to discount the real player (currently it's assumed that there will be only one real player)
    let neededRefill = minComputerPlayerCount - (Set.size alivePlayers - 1)
    in
    if neededRefill > 0
        then
            let
                randomXs = take neededRefill $ randomRs (0, windowWidth - 1) randomNumberGenerator
                randomYs = take neededRefill $ randomRs (0, windowHeight - 1) randomNumberGenerator
                newPlayers = [
                        generateNewPlayerAtRandomLocation randomNumberGenerator alivePlayers alivePowerups nextId
                    ]
            in 
                (Set.union alivePlayers (fromList newPlayers), nextId + 1)
        else
            (alivePlayers, nextId)

--Generates a player at a random initial position such that it doesn't collide with any other players or powerups
generateNewPlayerAtRandomLocation :: StdGen -> Set Player -> Set Powerup -> Int -> Player
generateNewPlayerAtRandomLocation rnGenerator currentPlayers currentPowerups id =

    let
        allPlayerCircles = [playerCircle p | p <- toList currentPlayers]
        allPowerupCircles = [powerupCircle p | p <- toList currentPowerups]
        allCircles = allPlayerCircles ++ allPowerupCircles
        newCircle = generateNonconflictingCircle rnGenerator defaultPlayerSize allCircles
    in
        Player {
            playerId = id,
            playerName = "",
            playerCircle = newCircle,
            playerSpeed = defaultXSpeed,
            dir = Vector2D 1.0 1.0
        }

generateNonconflictingCircle :: StdGen -> Float -> [Circle] -> Circle
generateNonconflictingCircle rnGenerator radius circles =
    let
        newCircle = generateRandomCircle rnGenerator radius
    in
        if  any (checkCircleIntersection newCircle) circles then
            generateNonconflictingCircle rnGenerator radius circles
        else
            newCircle

generateRandomCircle :: StdGen -> Float -> Circle
generateRandomCircle rnGenerator radius =
    let
        pX = take 100 $ randomRs (0, windowWidth - 1) rnGenerator
        pY = take 50 $ randomRs (0, windowHeight - 1) rnGenerator
        pLoc = Vector2D (fromIntegral $ last pX) (fromIntegral $ last pY)
    in
        Model.Circle {
            location = pLoc,
            -- TODO: generate player with random colour every time
            colour = rgbaOfColor green,
            size = radius
        }