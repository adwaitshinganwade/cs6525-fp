module GraphicsUtils where
import Graphics.Gloss

-- Setting up the main window
windowWidth, windowHeight, topleftX, topleftY :: Int
windowWidth = 800
windowHeight = 800
topleftX = 960 - (windowWidth `div` 2)
topleftY = 540 - (windowHeight `div` 2)

background :: Color
background = white

window :: String -> Display
window windowName = InWindow windowName (windowWidth, windowHeight) (topleftX, topleftY)

-- | Performs translation on the X and Y co-ordinates of a visual to
-- shift the origin (0, 0) from the center of the screen to the top-left corner
translateAsPerWorldCoordinates:: Float -> Float -> Picture -> Picture
translateAsPerWorldCoordinates x y picture =
    let xTranslation = worldXCenter - x
        yTranslation = worldYCenter - y
    in
        translate (-xTranslation) yTranslation picture
    where
        worldXCenter = fromIntegral windowHeight / 2
        worldYCenter = fromIntegral windowWidth / 2

drawASolidCircle :: Float -> Float -> Float -> Color -> Picture
drawASolidCircle radius x y col= translateAsPerWorldCoordinates x y $ color col (circleSolid radius)

drawAThickCircle :: Float -> Float -> Float -> Float -> Color -> Picture
drawAThickCircle radius thickness x y col = translateAsPerWorldCoordinates x y $ color col (thickCircle radius thickness)
