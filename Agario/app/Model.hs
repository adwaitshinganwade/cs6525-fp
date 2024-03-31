module Model where
import Graphics.Gloss

import GraphicsUtils
import qualified Graphics.Gloss

-- | The default x-velocity for player
defaultXSpeed = 6

-- | The default y-velocity for player
defaultYSpeed  = 0

-- | The radius of the circle for a regular powerup
regularPowerupSize = 10

{- Types and data -}
type Location = (Float, Float)

data Vector = Vector2D {
    xComponent :: Float,
    yComponent :: Float
} deriving Show

data Circle = Circle {
    location :: Location,
    colour :: Color,
    size :: Float
} deriving Show

data Player =  Player {
    playerCircle :: Circle,
    speed :: Model.Vector
} deriving Show

data Powerup = RegularPowerup {
   powerupCircle :: Circle,
   growthPotential :: Float
}

data Game = Agario {
    players :: [Player],
    powerups :: [Powerup]
}

drawPlayer :: Player -> Picture
drawPlayer player =
    drawASolidCircle radius x y col
    where
        c = playerCircle player
        radius = size c
        currentLocation = location c
        x = fst currentLocation
        y = snd currentLocation
        col = colour c

drawPowerup :: Powerup -> Picture
drawPowerup powerup =
    case powerup of
        RegularPowerup _ _ -> drawAThickCircle radius 5 x y col 
        where
            c = powerupCircle powerup
            radius = size c
            currentLocation = location c
            x = fst currentLocation
            y = snd currentLocation
            col = colour c


render :: Game -> Picture
render game =
    pictures $ map drawPlayer (players game) ++ map drawPowerup (powerups game)

initAgario :: Game
initAgario = Agario {
    players = [
        Player{playerCircle = Model.Circle{location = (50, 50), colour = dark red, size = 15}, speed = Vector2D{xComponent = defaultXSpeed + 5, yComponent = defaultYSpeed + 5}},
        Player{playerCircle = Model.Circle{location = (100, 100), colour = light blue, size = 30}, speed = Vector2D{xComponent = -(defaultXSpeed + 3), yComponent = defaultYSpeed + 3.5}},
        Player{playerCircle = Model.Circle{location = (150, 138), colour = yellow, size = 40}, speed = Vector2D{xComponent = defaultXSpeed + 1.5, yComponent = -(defaultYSpeed + 2.1)}}
    ],
    powerups = [
        RegularPowerup{powerupCircle = Model.Circle{location = (90, 85), colour = dark green, size = regularPowerupSize}, growthPotential = 2.0},
        RegularPowerup{powerupCircle = Model.Circle{location = (652, 128), colour = dark green, size = regularPowerupSize}, growthPotential = 2.0},
        RegularPowerup{powerupCircle = Model.Circle{location = (283, 571), colour = dark green, size = regularPowerupSize}, growthPotential = 2.0},
        RegularPowerup{powerupCircle = Model.Circle{location = (794, 387), colour = dark green, size = regularPowerupSize}, growthPotential = 2.0},
        RegularPowerup{powerupCircle = Model.Circle{location = (429, 104), colour = dark green, size = regularPowerupSize}, growthPotential = 2.0}
    ]
}

advancePlayers :: Float -> Game -> Game
advancePlayers elapsedSeconds currentState =
    currentState {players = movedPlayers}
    where
        movedPlayers = map advancePlayer $ players currentState


advancePlayer :: Player -> Player
advancePlayer currentPlayerState =
    currentPlayerState{playerCircle = c{location = (fromIntegral newX, fromIntegral newY)}}
    where
        playerSpeed = speed currentPlayerState
        c = playerCircle currentPlayerState
        newX = round (fst (location c) + xComponent playerSpeed) `mod` windowWidth
        newY = round (snd (location c) + yComponent playerSpeed) `mod` windowHeight