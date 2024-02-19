# Economancy Player #

A simple [Economancy](https://oflatt.github.io/economancy/) player that plays the game based on some static heuristics and random decision-making. 

## High-level Overview of How Decision are Made ##
The palyer is based on [this](https://users.cs.utah.edu/~mflatt/cs6525/economancy.html) specification.


### Invest Phase ###
The player tries to play it safe here. If the total attacking capability across all its cards is stronger than the total defending capability of at least half of the opponents, and if it has more coins than at least half the opponents, it chooses the smallest number of coins to secure the chance to attack (it may invest all available coins). If the above strategy cannot be implemented, it makes a random investment.

### Attack Phase ###
The player makes its next move in the attacking phase based on a very naive strategy: attack with an untapped card that has the highest attacking capability, and defend with an untapped card that has the highest defending capability.

Note:
- It is assumed that only the cards mentioned [here](https://users.cs.utah.edu/~mflatt/cs6525/economancy.html#%28tech._card._name%29) are used in a game. Hence, the player considers any card whose ``uses`` property is 0 as an untapped / available card.
- Since the Bubble card cannot atttack, it is ignored while choosing an attacking card.

### Buy Phase ###
The player chooses to invest based on a process that combines randomness with a set of fixed decisions. Based on a random number, the player chooses to either purchase the strongest affordable attacking card, the strongest affordable defending card, or to pass.

## Running the Player ##
The player has been compiled and tested on Windows. The player can be run by simply running ```EconomancyPlayer.exe``` in a Windows shell. The player accepts a JSON state (based on the specification) of the game from standard input. On each input, it outputs its decision as a singleton JSON array to standard output. It then waits for the next state. The player exits when it is passed a state with the ```end``` phase or when it does not recognize a phase.