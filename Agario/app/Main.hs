module Main where

import Graphics.Gloss
import GraphicsUtils
import Graphics.Gloss.Data.ViewPort (ViewPort(ViewPort))
import Model
import Controller
import System.Random (mkStdGen, getStdGen, StdGen)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = 
    do 
        seed <- round <$> getPOSIXTime
        play (window "Agario") background 30 (initAgario $ mkStdGen seed) render handleInput update
        where
            update :: Float -> Game -> Game
            update = updateGameState
            handleInput :: Event -> Game -> Game
            handleInput = handleKeyPress