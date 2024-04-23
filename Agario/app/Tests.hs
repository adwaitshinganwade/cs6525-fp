module Tests where

import Controller
import Graphics.Gloss (red, yellow, rgbaOfColor)
import Model (Circle (..), Vector (..), Player(..), Powerup(..), Game (thePlayer), regularPowerupSize)
import Data.Set(fromList, empty)
import Test.HUnit

-- Tests to validate circle-overlap logic

collisionTests = TestList [circleOverlapTest1, circleOverlapTest2, circleOverlapTest3, circleOverlapTest4, circleOverlapTest5]

collisionNonOverlapping = "The circles do not overlap. Circles are centered at: "

collisionOverlapping = "The circles overlap. Circles are centered at:"

testCircleOverlap :: Circle -> Circle -> Bool -> String -> Test
testCircleOverlap c1 c2 expectedResult errorMessage =
  TestCase
    ( assertEqual
        (errorMessage ++ show (location c1) ++ ", " ++ show (location c2))
        expectedResult
        (checkCircleIntersection c1 c2)
    )

-- Circles which clearly don't overlap
circleOverlapTest1 =
  testCircleOverlap
    Circle {location = Vector2D 1.0 1.0, size = 5, colour = rgbaOfColor red}
    Circle {location = Vector2D 10.0 15.0, size = 7, colour = rgbaOfColor red}
    False
    collisionNonOverlapping

-- Circles which clearly overlap
circleOverlapTest2 =
  testCircleOverlap
    Circle {location = Vector2D 1.0 1.0, size = 5, colour = rgbaOfColor red}
    Circle {location = Vector2D 1.0 1.0, size = 7, colour = rgbaOfColor red}
    True
    collisionOverlapping

-- Circles which intersect at exactly one point
circleOverlapTest3 =
  testCircleOverlap
    Circle {location = Vector2D 10.0 15.0, size = 7, colour = rgbaOfColor red}
    Circle {location = Vector2D 10.0 32.0, size = 10, colour = rgbaOfColor red}
    True
    collisionOverlapping

-- Circle one intersects circle two in its first quarter
circleOverlapTest4 =
  testCircleOverlap
    Circle {location = Vector2D 1.0 24.0, size = 10, colour = rgbaOfColor red}
    Circle {location = Vector2D 10.0 15.0, size = 7, colour = rgbaOfColor red}
    True
    collisionOverlapping

-- Circle two intersects circle one it its second quarter
circleOverlapTest5 =
  testCircleOverlap
    Circle {location = Vector2D 1.0 24.0, size = 10, colour = rgbaOfColor red}
    Circle {location = Vector2D 10.0 30.0, size = 7, colour = rgbaOfColor red}
    True
    collisionOverlapping


-- Tests to validate player-player collision

playerPlayerCollision = TestList [playerPlayerCollisionTest1, playerPlayerCollisionTest2]

-- Some players collide (possibly with multiple players)
playerPlayerCollisionTest1 =
  TestCase
    ( assertEqual
      "Failed to detect one or more dead players"
      expectedDeadPlayers
      (getPlayersCollidingWithAnotherPlayer activePlayers)
    )
  where
    collidingPlayer1 = Player {playerId = 1, playerName = "", playerCircle = Model.Circle {location = Vector2D 1.0 24.0, colour = rgbaOfColor red, size = 10}, playerSpeed = 10.0}
    collidingPlayer2 = Player {playerId = 2, playerName = "", playerCircle = Model.Circle {location = Vector2D 10.0 30.0, colour = rgbaOfColor yellow, size = 7}, playerSpeed = 15.0}
    collidingPlayer3 = Player {playerId = 3, playerName = "", playerCircle = Model.Circle {location = Vector2D 20.0 (-17.0), colour = rgbaOfColor red, size = 36}, playerSpeed = 36.0}
    activePlayers =
      fromList
        [ collidingPlayer1,
          collidingPlayer2,
          Player {playerId = 3, playerName = "", playerCircle = Model.Circle {location = Vector2D 150 138, colour = rgbaOfColor red, size = 40}, playerSpeed = 16.0},
          collidingPlayer3
        ]
    expectedDeadPlayers = fromList [collidingPlayer1, collidingPlayer2, collidingPlayer3]

-- No players collide
playerPlayerCollisionTest2 =
  TestCase
    ( assertEqual
      "Detected colliding players when none of them collide"
      expectedDeadPlayers
      (getPlayersCollidingWithAnotherPlayer activePlayers)
    )
  where
    activePlayers =
      fromList
        [ Player {playerId = 1, playerName = "", playerCircle = Model.Circle {location = Vector2D 1.0 24.0, colour = rgbaOfColor red, size = 10}, playerSpeed = 10.0},
          Player {playerId = 3, playerName = "", playerCircle = Model.Circle {location = Vector2D 150 138, colour = rgbaOfColor red, size = 40}, playerSpeed = 16.0},
          Player {playerId = 3, playerName = "", playerCircle = Model.Circle {location = Vector2D (-100) 138, colour = rgbaOfColor red, size = 24}, playerSpeed = 16.0},
          Player {playerId = 3, playerName = "", playerCircle = Model.Circle {location = Vector2D 150 (-138), colour = rgbaOfColor red, size = 70}, playerSpeed = 16.0}
        ]
    expectedDeadPlayers = empty

-- Tests to validate consumption of powerup by a player

playerCosumptionOfPowerupTests = TestList [playerPowerupConsumptionTest1, playerPowerupConsumptionTest2]


testPowerupConsumption :: Player -> Powerup -> Float -> Float -> Test
testPowerupConsumption thePlayer thePowerup expectedNewSize expectedNewSpeed =
    TestCase(
        assertEqual
        "The player could not consume food correctly"
         expectedPlayerState
        (playerEatsPowerup thePlayer thePowerup)
    )
    where
        currentCircle = playerCircle thePlayer
        expectedPlayerState = thePlayer {playerCircle = currentCircle{size = expectedNewSize}, playerSpeed = expectedNewSpeed}

-- Consuming a powerup whose effects can be fully applied
playerPowerupConsumptionTest1 =
    testPowerupConsumption aPlayer aPowerup 12 (10 / 12 * 10)
    where
        aPlayer = Player {playerId = 1, playerName = "", playerCircle = Model.Circle {location = Vector2D 1.0 24.0, colour = rgbaOfColor red, size = 10}, playerSpeed = 10.0}
        aPowerup = RegularPowerup{powerupId = 1, powerupCircle = Model.Circle{location = Vector2D 3.0 20.0, colour = (10, 10, 20, 10), size = regularPowerupSize}, growthPotential = 2.0}


-- Consuming a powerup which causes the size and speed of the player reach the cap
playerPowerupConsumptionTest2 =
    testPowerupConsumption aPlayer aPowerup maxPlayerSize minPlayerSpeed
    where
        aPlayer = Player {playerId = 1, playerName = "", playerCircle = Model.Circle {location = Vector2D 1.0 24.0, colour = rgbaOfColor red, size = 10}, playerSpeed = 10.0}
        aPowerup = RegularPowerup{powerupId = 1, powerupCircle = Model.Circle{location = Vector2D 3.0 20.0, colour = (10, 10, 20, 10), size = regularPowerupSize}, growthPotential = 191.2}

-- TODO: tests for consuming powerup
