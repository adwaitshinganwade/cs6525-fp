## Agario in Haskell ##

<br> 

![screenshot](agario-game.png)

<br>

This game is a spin-off of the popular game  [Agar.io](https://agar.io/). It has been developed using the [Gloss](https://hackage.haskell.org/package/gloss) graphics library. The basic mechanics of the game are as follows:

- The world is composed of players (solid circles 🔴) and powerups (rings 🔘). The game is a simplified version of the original: there is one human player ("the player") and a few computer players (number configurable in code).

- The player can be controlled using the up (⬆️), down (⬇️), left(⬅️) and right(➡️) arrow keys. The player is labeled with the string "You." The comptuer players keep moving randomly. A player gets wrapped around the world if it crosses an edge of the game window. 

- A player grows in size if it eats a powerup. This slows down the player as its mass increases on eating a powerup. The game maintains a minimum number of powerups (configurable in code).

- If a player runs into another player, both players die. If the human player dies, the game ends. If two computer players die, new ones are spawned in the next frames. 

## Build Requirements ##
The game has been built with the following:
- cabal: v3.10.2.1
- Haskell: GHC v9.4.8

To build the game, run the following command in a terminal opened in the directory containing ```agario.cabal```: ```cabal build```

To run the built executable, run ```cabal run```. For convenience, the Linux executable for the game is available in dist/agario. The executable can be run using: ```./agario``` (assuming the terminal is opened from the directory containing it).

## Design Decisions ##
- An add-on to the game was enabling networking support. This could not be done due to time contraints. However, the code has been written following the MVC design pattern. We therefore have the provision to convert it into a client-server game.
- New players are generated such that they don't collide with other players and powerups. New powerups are created at random locations, which can lead to overlapping powerups.  
- The logic that handles the physics of the game has been unit-tested. The tests can be found in ```tests/Tests.hs```. Tests can be launched using ```cabal test```. 
