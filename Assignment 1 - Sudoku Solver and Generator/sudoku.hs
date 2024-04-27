module SudokuSolver where
import Data.List (transpose, permutations, find)
import Data.Sequence (chunksOf)
import System.Random
import Data.Time.Clock.POSIX (getPOSIXTime)

-- number of rows
m :: Int = 3

-- number of columns
n :: Int = 3

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
        case find (\sol -> not $ null sol) [solveSudoku' x row (col + 1) | x <- generateAllValidPossibilities board row col] of
            Just solution -> solution
            Nothing -> []
    | otherwise = solveSudoku' board row (col + 1)

{-
    GENERATOR
-}

-- maximum number of empty cells
emptyCells :: Int = 40

generateSudokuPuzzle :: Int -> StdGen -> [[Int]]
generateSudokuPuzzle numBlankCells rnGen =
    let
        (validBoard, couldGenerate) = generateAValidSudokuBoard generateEmptySudokuBoard 0 0
        randomNums = take (m*n*m*n) $ randomRs (0, m*n-1) rnGen
        rowIdxs = take numBlankCells randomNums
        colIdxs = take numBlankCells $ drop numBlankCells randomNums
        blankCellIdxs = zip rowIdxs colIdxs
    in
        if couldGenerate then
            foldl (\config cellIdx  -> uncurry (fillCellAndGetBoard config) cellIdx 0) validBoard blankCellIdxs
        else
            []

generateAValidSudokuBoard :: [[Int]] -> Int -> Int -> ([[Int]], Bool)
generateAValidSudokuBoard currentBoard nextRowToGenerate nextColToGenerate
  | nextRowToGenerate == m*n = (currentBoard, True)
  | nextColToGenerate == m*n = generateAValidSudokuBoard currentBoard (nextRowToGenerate + 1) 0
  | otherwise = let
                    nextValidConfigurations = generateAllValidPossibilities currentBoard nextRowToGenerate nextColToGenerate
                in
                if null nextValidConfigurations
                    then
                        (currentBoard, False)
                    else
                        case find (\config -> snd config && checkValidityOfAllCells (fst config) nextRowToGenerate) [
                            generateAValidSudokuBoard possibility nextRowToGenerate (nextColToGenerate + 1)
                            | possibility <- nextValidConfigurations] of
                                Just (vBoard, vTruth) -> (vBoard, True)
                                Nothing -> (currentBoard, False)

checkValidityOfAllCells :: [[Int]] -> Int -> Bool
checkValidityOfAllCells board tillRow =
        all (\x -> x) [cellValueValid row col board | row <- [0..tillRow], col <- [0..(m*n-1)]]


generateEmptySudokuBoard :: [[Int]]
generateEmptySudokuBoard =
    replicate (m*n) (replicate (n*m) 0)

generatePuzzle = do
    seed <- round <$> getPOSIXTime
    mapM_ print (generateSudokuPuzzle emptyCells (mkStdGen seed))