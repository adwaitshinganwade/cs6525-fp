module Tests where

import Controller
import Graphics.Gloss (red, yellow, rgbaOfColor)
import Model (Circle (..), Vector (..), Player(..))
import Data.Set(fromList, empty)
import Test.HUnit

collisionTests = TestList [circleOverlapTest1, circleOverlapTest2, circleOverlapTest3, circleOverlapTest4, circleOverlapTest5]

collisionNonOverlapping = "The circles do not overlap. Circles are centered at: "

collisionOverlapping = "The circles overlap. Circles are centered at:"

testCircleOverlap :: Circle -> Circle -> Bool -> String -> Test
testCircleOverlap c1 c2 expectedResult errorMessage =
  TestCase
    ( assertEqual
        (errorMessage ++ show (location c1) ++ ", " ++ show (location c2))
        (checkCircleIntersection c1 c2)
        expectedResult
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

playerPlayerCollision = TestList [playerPlayerCollisionTest1, playerPlayerCollisionTest2]

-- Some players collide (possibly with multiple players)
playerPlayerCollisionTest1 =
  TestCase
    ( assertEqual
      "Failed to detect one or more dead players"
      (getPlayersCollidingWithAnotherPlayer activePlayers)
      expectedDeadPlayers
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
      (getPlayersCollidingWithAnotherPlayer activePlayers)
      expectedDeadPlayers
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