module Controller where
import Model (Circle (..), Vector (xComponent, yComponent))


checkCircleIntersection :: Circle -> Circle -> Bool
checkCircleIntersection aCircle anotherCircle =
    let 
        radiusOne = size aCircle        
        radiusTwo = size anotherCircle
    in
        (distance <= radiusOne + radiusTwo)
    where
        centerOne = location aCircle
        centerTwo = location anotherCircle
        distance = sqrt $ (xComponent centerOne - xComponent centerTwo)**2 + (yComponent centerOne - yComponent centerTwo)**2
    
