# Economancy Player #

A simple [Economancy](https://oflatt.github.io/economancy/) player that plays the game based on some static heuristics and random decision-making. 

## High-level Overview of How Decision are Made ##
The palyer is based on [this](https://users.cs.utah.edu/~mflatt/cs6525/economancy.html) specification.


### Invest Phase ###
The player tries to play it safe here. If the total attacking capability across all its cards is stronger than the total defending capability of at least half of the opponents, and if it has more coins than at least half the opponents, it chooses the smallest number of coins to secure the chance to attack (it may invest all available coins). If the above strategy cannot be implemented, it makes a random investment.

### Attack Phase ###
The player makes its next move in the attacking phase based on a very naive strategy: attack with an untapped card that has the highest attacking capability, and defend with an untapped card that has the highest defending capability.

Note:
- It is assumed that all the cards mentioned [here](https://users.cs.utah.edu/~mflatt/cs6525/economancy.html#%28tech._card._name%29) are used in a game. Hence, the player considers any card whose ``uses`` property is 0 as an untapped / available card.
- Since the Bubble card cannot atttack, it is ignored while choosing an attacking card.
- While defending, a Wall of Wealth card is considered untapped if its uses are <=1 since it can be used twice in a turn to defend.

### Buy Phase ###
The player chooses to invest based on a process that combines randomness with a set of fixed decisions. Based on a random number, the player chooses to either:
- purchase a (there might be many) strongest affordable attacking card
- purchase a strongest affordable defending card
- purchase a card that earns it the highest income on the next day
-  or to pass

## Running the Player ##
The player has been compiled and tested on Windows and Ubuntu. 

The Windows player can be run by simply running ```EconomancyPlayer.exe``` in a Windows shell. 

The Linux player can be run as follows in a Linux terminal: ```./EconomancyPlayer```

 The player accepts a JSON state (based on the specification) of the game from standard input. On each input, it outputs its decision as a singleton JSON array to standard output. It then waits for the next state. The player exits when it is passed a state with the ```end``` phase or when it does not recognize a phase.

## References ##
1. [Learn You a Haskell for Great Good!](https://learnyouahaskell.com/chapters)
2. [Hackage](https://hackage.haskell.org/) documentation for standard library functions
3. JSON in Haskell:
    - https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html
    - https://www.youtube.com/watch?v=IMlDZNWTurw

