Both the Sudoku solver and generator can be run using ```ghci```. For both, the variables `m` nd `n` define the number of rows and columns in a Sudoku grid. There variables should be set as per the desired output (lines 8 and 11 in ```Sudoku.hs```).
## Sudoku Solver ##
The solver can be run through a terminal opened in the directory containing ```sudoku.hs```.

```ghci>``` :l sudoku.hs

```ghci>``` solveSudoku \<nested-lists-representing-puzzle\>

e.g.

```ghci>``` solveSudoku [[0, 0, 2, 0, 1, 0, 0, 6, 9],[4, 0, 0, 0, 0, 9, 0, 8, 0],[9, 0, 6, 4, 2, 8, 7, 0, 5],[0, 4, 0, 3, 7, 6, 1, 5, 0],[7, 3, 0, 5, 0, 0, 0, 2, 0],[0, 0, 1, 0, 0, 0, 3, 4, 0],[5, 0, 0, 0, 0, 0, 0, 9, 0],[8, 9, 4, 0, 6, 0, 0, 1, 0],[0, 0, 0, 9, 5, 4, 0, 0, 2]]

Output

[[3,8,2,7,1,5,4,6,9],[4,7,5,6,3,9,2,8,1],[9,1,6,4,2,8,7,3,5],[2,4,9,3,7,6,1,5,8],[7,3,8,5,4,1,9,2,6],[6,5,1,8,9,2,3,4,7],[5,2,7,1,8,3,6,9,4],[8,9,4,2,6,7,5,1,3],[1,6,3,9,5,4,8,7,2]]

## Sudoku Generator ##
The generator can be run through a terminal opened in the directory containing ```sudoku.hs```.

```ghci>``` generatePuzzle

This will generate a random puzzle based on `m` (line 8), `n` (line 11) and `emptyCells` (line 124) ```Sudoku.hs```

e.g.

```ghci>``` generatePuzzle (`m` = 3, `n` = 3, `emptyCells` = 40)

[1,0,0,0,5,0,0,8,0] <br>
[0,5,6,7,8,0,0,2,3] <br>
[7,0,0,1,2,0,4,5,6] <br>
[0,0,4,3,6,5,8,9,7] <br>
[3,6,0,8,9,7,2,0,4] <br>
[0,9,7,0,1,4,3,6,0] <br>
[0,3,0,6,0,2,9,7,8] <br>
[6,0,0,9,0,8,5,3,1] <br>
[9,0,8,5,0,1,0,0,0]