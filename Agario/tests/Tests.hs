module Main where

import Controller
import Graphics.Gloss (red, yellow, rgbaOfColor)
import Model
import Data.Set hiding(size)
import qualified Data.Set(size)
import Test.HUnit
import Data.List (tail, last)
import System.Random (getStdGen, mkStdGen)
import qualified System.Exit as Exit

main :: IO()
main = do
    result <- runTestTT (TestList [collisionTests, playerPlayerCollisionTests, powerupConsumptionDetectionTests, minimumPowerupCountTests])
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

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

playerPlayerCollisionTests = TestList [playerPlayerCollisionTest1, playerPlayerCollisionTest2]

-- Some players collide (possibly with multiple players)
playerPlayerCollisionTest1 =
  TestCase
    ( assertEqual
      "Failed to detect one or more dead players"
      expectedDeadPlayers
      (getPlayersCollidingWithAnotherPlayer activePlayers)
    )
  where
    collidingPlayer1 = Player {playerId = 1, playerName = "", playerCircle = Model.Circle {location = Vector2D 1.0 24.0, colour = rgbaOfColor red, size = 10}, playerSpeed = 10.0, dir = Vector2D 1.0 1.0}
    collidingPlayer2 = Player {playerId = 2, playerName = "", playerCircle = Model.Circle {location = Vector2D 10.0 30.0, colour = rgbaOfColor yellow, size = 7}, playerSpeed = 15.0, dir = Vector2D 1.0 1.0}
    collidingPlayer3 = Player {playerId = 3, playerName = "", playerCircle = Model.Circle {location = Vector2D 20.0 (-17.0), colour = rgbaOfColor red, size = 36}, playerSpeed = 36.0, dir = Vector2D 1.0 1.0}
    activePlayers =
      fromList
        [ collidingPlayer1,
          collidingPlayer2,
          Player {playerId = 3, playerName = "", playerCircle = Model.Circle {location = Vector2D 150 138, colour = rgbaOfColor red, size = 40}, playerSpeed = 16.0, dir = Vector2D 1.0 1.0},
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
        [ Player {playerId = 1, playerName = "", playerCircle = Model.Circle {location = Vector2D 1.0 24.0, colour = rgbaOfColor red, size = 10}, playerSpeed = 10.0, dir = Vector2D 1.0 1.0},
          Player {playerId = 3, playerName = "", playerCircle = Model.Circle {location = Vector2D 150 138, colour = rgbaOfColor red, size = 40}, playerSpeed = 16.0, dir = Vector2D 1.0 1.0},
          Player {playerId = 3, playerName = "", playerCircle = Model.Circle {location = Vector2D (-100) 138, colour = rgbaOfColor red, size = 24}, playerSpeed = 16.0, dir = Vector2D 1.0 1.0},
          Player {playerId = 3, playerName = "", playerCircle = Model.Circle {location = Vector2D 150 (-138), colour = rgbaOfColor red, size = 70}, playerSpeed = 16.0, dir = Vector2D 1.0 1.0}
        ]
    expectedDeadPlayers = empty

-- Tests for consuming powerup
powerupConsumptionDetectionTests = TestList [playerPowerupConsumptionDetectionTest1, playerPowerupConsumptionDetectionTest2, playerPowerupConsumptionDetectionTest3]

testPowerupConsumptionDetection :: [Player] -> [Powerup] -> Set (Player, Powerup) -> Test
testPowerupConsumptionDetection alivePlayers alivePowerups expectedPlayerPowerupPairs =
    TestCase (
        assertEqual
        "One or more player-powerup pairs were not detected correctly"
         expectedPlayerPowerupPairs
        (getEatenPowerups alivePlayers alivePowerups)
    )

-- Each player eats a powerup, no ambiguity
playerPowerupConsumptionDetectionTest1 =
  let
    alivePlayers =
      [
        Player {playerId = 1, playerName = "", playerCircle = Model.Circle {location = Vector2D 5.0 4.0, colour = rgbaOfColor red, size = 4.0}, playerSpeed = 10.0, dir = Vector2D 1.0 1.0},
        Player {playerId = 3, playerName = "", playerCircle = Model.Circle {location = Vector2D (-5.0) 3.0, colour = rgbaOfColor red, size = 3.0}, playerSpeed = 16.0, dir = Vector2D 1.0 1.0}
      ]
    alivePowerups =
      [
       RegularPowerup{powerupId = 1, powerupCircle = Model.Circle{location = Vector2D 6.0 (-1.0), colour = (10, 10, 20, 10), size = 3.0}, growthPotential = 191.2},
       RegularPowerup{powerupId = 1, powerupCircle = Model.Circle{location = Vector2D (-7.0) 1.0, colour = (10, 10, 20, 10), size = 2.0}, growthPotential = 191.2}
      ]
    expectedPlayerPowerupPairs = fromList $ zip alivePlayers alivePowerups
  in
    testPowerupConsumptionDetection alivePlayers alivePowerups expectedPlayerPowerupPairs


-- No powerups are consumed
playerPowerupConsumptionDetectionTest2 =
  let
    alivePlayers =
      [
        Player {playerId = 1, playerName = "", playerCircle = Model.Circle {location = Vector2D 5.0 4.0, colour = rgbaOfColor red, size = 4.0}, playerSpeed = 10.0, dir = Vector2D 1.0 1.0},
        Player {playerId = 3, playerName = "", playerCircle = Model.Circle {location = Vector2D (-5.0) 7.0, colour = rgbaOfColor red, size = 3.0}, playerSpeed = 16.0, dir = Vector2D 1.0 1.0}
      ]
    alivePowerups =
      [
       RegularPowerup{powerupId = 1, powerupCircle = Model.Circle{location = Vector2D 6.0 (-5.0), colour = (10, 10, 20, 10), size = 3.0}, growthPotential = 191.2},
       RegularPowerup{powerupId = 1, powerupCircle = Model.Circle{location = Vector2D (-7.0) 1.0, colour = (10, 10, 20, 10), size = 2.0}, growthPotential = 191.2}
      ]
  in
    testPowerupConsumptionDetection alivePlayers alivePowerups empty


-- Multiple players colliding with the same powerup. The first one in the set of players eats
playerPowerupConsumptionDetectionTest3 =
  let
    alivePlayers =
      [
        Player {playerId = 1, playerName = "", playerCircle = Model.Circle {location = Vector2D 5.0 4.0, colour = rgbaOfColor red, size = 4.0}, playerSpeed = 10.0, dir = Vector2D 1.0 1.0},
        Player {playerId = 3, playerName = "", playerCircle = Model.Circle {location = Vector2D 11.0 (-1.0), colour = rgbaOfColor red, size = 3.0}, playerSpeed = 16.0, dir = Vector2D 1.0 1.0}
      ]
    alivePowerups =
      [
       RegularPowerup{powerupId = 1, powerupCircle = Model.Circle{location = Vector2D 6.0 (-1.0), colour = (10, 10, 20, 10), size = 3.0}, growthPotential = 191.2},
       RegularPowerup{powerupId = 1, powerupCircle = Model.Circle{location = Vector2D (-7.0) 1.0, colour = (10, 10, 20, 10), size = 2.0}, growthPotential = 191.2}
      ]
    expectedPlayerPowerupPairs = fromList [(head alivePlayers, head alivePowerups)]
  in
    testPowerupConsumptionDetection alivePlayers alivePowerups expectedPlayerPowerupPairs

-- Tests to validate the logic to ensure minimum powerups

-- Less than minimum number of powerups

minimumPowerupCountTests = TestList [minimumPowerupsTest1, minimumPowerupsTest2, minimumPowerupsTest3, minimumPowerupsTest4]

minimumPowerupsTest1 =
      TestCase (
        assertBool
        "Failed to replenish powerups"
        (Data.Set.size (fst (ensureMinimumPowerupCount (generateRandomPowerups 13) 13 (mkStdGen 1000))) >= minPowerupCount)
        )

-- Powerup count is exactly minPowerupCount
minimumPowerupsTest2 =
      TestCase (
        assertBool
        "Created more powerups than necessary"
        (snd (ensureMinimumPowerupCount (generateRandomPowerups minPowerupCount) 13 (mkStdGen 1000)) == 13)
        )

-- More than minimum required powerups already exist
minimumPowerupsTest3 =
      TestCase (
        assertBool
        "Created more powerups than necessary"
        (snd (ensureMinimumPowerupCount (generateRandomPowerups (minPowerupCount + 5)) 17 (mkStdGen 1000)) == 17)
        )

-- The correct next ID is returned after creating powerups
minimumPowerupsTest4 =
      TestCase (
        assertBool
        "Created more powerups than necessary"
        (snd (ensureMinimumPowerupCount (generateRandomPowerups (minPowerupCount - 5)) 13 (mkStdGen 1000)) == 14)
        )


generateRandomPowerups :: Int -> Set Powerup
generateRandomPowerups numPowerups =
  let
    someXs = [1..numPowerups]
    someYs = [1..numPowerups]
  in
    fromList [
       RegularPowerup{
                        powerupId = i,
                        powerupCircle = Model.Circle{
                            location = Vector2D (fromIntegral $ someXs !! i) (fromIntegral $ someYs !! i),
                            -- TODO (low priority): random colour for each powerup
                            -- TODO: powerups should not be generated at a position where there's already something
                            colour = rgbaOfColor yellow,
                            size = regularPowerupSize},
                        growthPotential = regularPowerupGrowthPotential
                    }
        | i <- [0..(numPowerups-1)]
    ]