import Data.List (transpose)
import Data.Sequence (chunksOf)

-- number of rows
m :: Int = 2

-- number of columns
n :: Int = 2

candidates :: [Int] = [1 .. m*n]

getRows :: [[Int]] -> [[Int]]
getRows board = board

-- Returns all columns in a board
getColumns :: [[Int]] -> [[Int]]
getColumns = transpose

-- Returns all subgrids in the board as a list of lists
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

-- returns the grid number based on the cell
getGridNumberFromCell :: Int -> Int -> Int
getGridNumberFromCell row col =
    ((row `div` n) * n) + (col `div` m)

-- Checks if the board is valid
isBoardValid :: [[Int]] -> Bool
isBoardValid board =
    all validArrangement (getRows board) &&
    all validArrangement (getColumns board) &&
    all validArrangement (getGrids board)

validArrangement :: [Int] -> Bool
validArrangement arrangement =
    (length arrangement == m*n) && validArrangement' candidates arrangement

-- Checks if a list has each element integer from 1*m
validArrangement' :: [Int] -> [Int] -> Bool
validArrangement' wantedNums arrangement
  = foldr
      (\ number -> (&&) (number `elem` arrangement)) True wantedNums

hasDuplicates :: [Int] -> Bool
hasDuplicates [] = False
hasDuplicates (n:ns)
    | n == 0 = hasDuplicates ns
    |otherwise = elem n ns || hasDuplicates ns


-- Checks if the value in a given cell is valid as per row, column, and subgrid
cellValueValid :: Int -> Int -> [[Int]] -> Bool
cellValueValid row col board = not (hasDuplicates (getRows board !! row))
    && not (hasDuplicates (getColumns board !! col))
    && not (hasDuplicates (getGrids board !! getGridNumberFromCell row col))

cellEmpty :: [[Int]] -> Int -> Int -> Bool
cellEmpty board row col = ((board !! row) !! col) == 0

generateAllValidPossibilities :: [[Int]] -> Int -> Int -> [[[Int]]]
generateAllValidPossibilities board row col =
    filter (cellValueValid row col) [fillCellAndGetBoard board row col x | x <- candidates]

fillCellAndGetBoard :: [[Int]] -> Int -> Int -> Int -> [[Int]]
fillCellAndGetBoard board row col number
    | row == 0 = (fillNumberInRow (head board) col number) : tail board
    | otherwise = fst dissectedBoard ++
        ((fillNumberInRow (head (snd dissectedBoard)) col number) : tail (snd dissectedBoard))
    where dissectedBoard = splitAt row board

fillNumberInRow :: [Int] -> Int -> Int -> [Int]
fillNumberInRow row col number
    | col == 0 = number : tail row
    | otherwise = fst dissectedRow ++ (number : tail (snd dissectedRow))
    where dissectedRow = splitAt col row

replaceFirstEmptyCellInRow :: [Int] -> Int -> [Int]
replaceFirstEmptyCellInRow [] number = []
replaceFirstEmptyCellInRow (cell: restCells) number =
    if cell == 0
        then number : restCells
        else cell : replaceFirstEmptyCellInRow restCells number

solveSudoku :: [[Int]] -> [[Int]]
solveSudoku board =
    solveSudoku' board 0 0

solveSudoku' :: [[Int]] -> Int -> Int -> [[Int]]
solveSudoku' board row col
    -- Worked on all rows, we have a solution!
    | row == m*n = board
    -- Reached the end of a row, start with the next one
    | col == m*n = solveSudoku' board (row + 1) 0
    -- At some cell on the board. Fill it if it's empty
    | cellEmpty board row col =
        let solutions = [solveSudoku' x row (col + 1) | x <- generateAllValidPossibilities board row col]
        in if null solutions then [[]] else head solutions
    | otherwise = solveSudoku' board row (col + 1)

