module Main where

import Graphics.Gloss
import Model(Game, advancePlayers, initAgario, render)
import GraphicsUtils
import Graphics.Gloss.Data.ViewPort (ViewPort(ViewPort))

main :: IO ()
main = 
    simulate (window "Agario") background 30 initAgario render update
    where
        update :: ViewPort -> Float -> Game -> Game
        update _ = advancePlayers

        
    -- animate (window "Agario") background frame
    -- where
    --     frame :: Float -> Picture
    --     frame seconds = render $ advancePlayers seconds initAgario
