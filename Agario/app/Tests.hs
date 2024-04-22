module Tests where
import Test.HUnit
import Controller (checkCircleIntersection)
import Model (Circle(..), Vector(..))
import Graphics.Gloss (red)

collisionTests = TestList [testNonOverlappingCircles1, testNonOverlappingCircles2]

testNonOverlappingCircles1 = TestCase (assertEqual 
    "Non-overlapping circles" 
    (checkCircleIntersection Circle{location = Vector2D 10 10, size = 3, colour = red} Circle{location = Vector2D 25 30, size = 10, colour = red}) 
    False)


testNonOverlappingCircles2 = TestCase (assertEqual 
    "Non-overlapping circles" 
    (checkCircleIntersection Circle{location = Vector2D 10 10, size = 3, colour = red} Circle{location = Vector2D 5 7, size = 10, colour = red}) 
    True)