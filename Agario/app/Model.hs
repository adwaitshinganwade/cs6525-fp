module Model where
import Graphics.Gloss hiding (Vector)

import GraphicsUtils
import qualified Graphics.Gloss hiding (Vector)
import Data.Set (Set, fromList, empty, toList)

-- | The default x-velocity for player
defaultXSpeed = 6

-- | The default y-velocity for player
defaultYSpeed  = 0

-- | The radius of the circle for a regular powerup
regularPowerupSize = 10
regularPowerupGrowthPotential :: Float = 2.0

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
    playerSpeed :: Float
} deriving (Show, Eq, Ord)

data Powerup = RegularPowerup {
   powerupId :: Int,
   powerupCircle :: Circle,
   growthPotential :: Float
} deriving (Show, Eq, Ord)

data Game = Agario {
    thePlayer :: Int,
    players :: Set Player,
    deadPlayers :: Set Player,
    powerups :: Set Powerup,
    eatenPowerups :: Set Powerup
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


render :: Game -> Picture
render game =
    pictures $ map drawPlayer (toList $ players game) ++ map drawPowerup (toList $ powerups game)

initAgario :: Game
initAgario = Agario {
    players = fromList [
        Player{ playerId = 1, playerName = "", playerCircle = Model.Circle{location = Vector2D 50 50, colour = (10, 10, 10, 10) , size = 15}, playerSpeed = 10.0},
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