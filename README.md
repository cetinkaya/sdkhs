# sdkhs
Sudoku Solver in Haskell


###### Example usage:

Import the module(`Sdk`) and use `solve` function to solve a sudoku puzzle. You can print the solution nicely with `printNicely`. A sudoku puzzle is represented by a list of lists of integers. Empty positions are represented with 0.

```Haskell
module Main (main) where

import Sdk

puzzle :: [[Int]]
puzzle = [[1, 2, 0, 4, 5, 6, 7, 8, 9],
          [4, 5, 0, 7, 8, 9, 1, 0, 3],
          [7, 8, 9, 1, 2, 3, 4, 5, 6],
          [2, 3, 0, 5, 6, 7, 8, 0, 1],
          [5, 6, 7, 0, 9, 0, 2, 3, 4],
          [0, 9, 1, 0, 3, 4, 5, 0, 0],
          [0, 4, 0, 0, 7, 8, 9, 1, 2],
          [6, 7, 8, 0, 1, 0, 3, 0, 5],
          [0, 1, 0, 0, 4, 5, 6, 7, 8]]

main :: IO ()
main = printNicely $ solve puzzle
```

