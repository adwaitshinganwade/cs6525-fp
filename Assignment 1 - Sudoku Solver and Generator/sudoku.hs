import Data.List (transpose)
import Data.Sequence (chunksOf)

-- number of rows
m :: Int = 2

-- number of columns
n :: Int = 2

candidates :: [Int] = [1 .. m*n]

subgrid :: [[Int]] = []

validArrangement :: [Int] -> Bool
validArrangement arrangement =
    (length arrangement == m*n) && validArrangement' candidates arrangement

-- Checks if a list has each element integer from 1*m
validArrangement' :: [Int] -> [Int] -> Bool
validArrangement' wantedNums arrangement
  = foldr
      (\ number -> (&&) (number `elem` arrangement)) True wantedNums

-- Returns all elements in a column as a list
getColumn :: Int -> [[Int]] -> [Int]
getColumn col grid
    | col < n*m = [row !! col | row <- grid]
    | otherwise = []

-- Returns all columns in a board
getColumns :: [[Int]] -> [[Int]]
getColumns = transpose

getGrids :: [[Int]] -> [[Int]]
getGrids [] = []
getGrids board =
      getGridsInRow (take n board) ++ getGrids (remainingRows board)
        where
            remainingRows = drop n



getGridsInRow :: [[Int]] -> [[Int]]
getGridsInRow ([]:lsts) = []
getGridsInRow rowChunk = getAGrid rowChunk : getGridsInRow (remainingChunks rowChunk)
        where
            remainingChunks = map (drop m)

getAGrid :: [[Int]] -> [Int]
getAGrid = concatMap (take m)