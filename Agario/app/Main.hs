module Main where

import Graphics.Gloss
import GraphicsUtils
import Graphics.Gloss.Data.ViewPort (ViewPort(ViewPort))
import Model
import Controller
import System.Random (mkStdGen, getStdGen, StdGen)
import Data.Time.Clock.POSIX (getPOSIXTime)

main :: IO ()
main = 
    do 
        simulate (window "Agario") background 30 (initAgario $ mkStdGen 2000) render update
        where
            update :: ViewPort -> Float -> Game -> Game
            -- update _ = advancePlayers
            update _ = updateGameState

        
    -- animate (window "Agario") background frame
    -- where
    --     frame :: Float -> Picture
    --     frame seconds = render $ advancePlayers seconds initAgario
