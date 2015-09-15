module Sdk where

import Data.List
import Control.Monad (msum, forM_)

testCase :: [[Int]]
testCase = [[1, 2, 3, 4, 5, 6, 7, 8, 9],
            [4, 5, 6, 7, 8, 9, 1, 2, 3],
            [7, 8, 9, 1, 2, 3, 4, 5, 6],
            [2, 3, 4, 5, 6, 7, 8, 9, 1],
            [5, 6, 7, 8, 9, 1, 2, 3, 4],
            [8, 9, 1, 2, 3, 4, 5, 6, 7],
            [3, 4, 5, 6, 7, 8, 9, 1, 2],
            [6, 7, 8, 9, 1, 2, 3, 4, 5],
            [9, 1, 2, 3, 4, 5, 6, 7, 8]]

testCase2 :: [[Int]]
testCase2 = [[1, 2, 0, 4, 5, 6, 7, 8, 9],
             [4, 5, 0, 7, 8, 9, 1, 0, 3],
             [7, 8, 9, 1, 2, 3, 4, 5, 6],
             [2, 3, 0, 5, 6, 7, 8, 0, 1],
             [5, 6, 7, 0, 9, 0, 2, 3, 4],
             [0, 9, 1, 0, 3, 4, 5, 0, 0],
             [0, 4, 0, 0, 7, 8, 9, 1, 2],
             [6, 7, 8, 0, 1, 0, 3, 0, 5],
             [0, 1, 0, 0, 4, 5, 6, 7, 8]]

testCase3 :: [[Int]]
testCase3 = [[0, 0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0, 0]]

testCase4 :: [[Int]]
testCase4 = [[0, 0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0, 0],
             [0, 0, 0, 0, 0, 0, 0, 0, 1]]
            
row :: [[Int]] -> (Int, Int) -> [Int]
row lst (i, _) = lst !! i
            
col :: [[Int]] -> (Int, Int) -> [Int]
col lst (_, j) = map (!! j) lst

grp :: [[Int]] -> (Int, Int) -> [Int]
grp lst (i, j) = concatMap (take 3 . drop jj) rows 
    where rows = take 3 $ drop ii lst
          ii = 3 * div i 3
          jj = 3 * div j 3

rcg :: [[Int]] -> (Int, Int) -> [Int]
rcg lst pos = union g $ union r c
    where r = row lst pos
          c = col lst pos
          g = grp lst pos

possible :: [[Int]] -> (Int, Int) -> [Int]
possible lst pos = [1..9] \\ rcg lst pos

value :: [[Int]] -> (Int, Int) -> Int                            
value lst (i, j) = (lst !! i) !! j

permuteZip :: [a] -> [b] -> [(a, b)]                 
permuteZip l1 l2 = concatMap (\v1 -> map (\v2 -> (v1, v2)) l2) l1

emptyPositions :: [[Int]] -> [(Int, Int)]
emptyPositions lst = filter ((== 0) . value lst) allPos
  where allPos = permuteZip [0..8] [0..8]

setValue :: [[Int]] -> (Int, Int) -> Int -> [[Int]]
setValue lst (i, j) newVal = pre ++ [newRow] ++ post
  where pre = take i lst
        post = drop (i + 1) lst
        oldRow = lst !! i
        newRow = take j oldRow ++ [newVal] ++ drop (j + 1) oldRow

firstSomething :: [Maybe a] -> Maybe a
firstSomething = msum
                       
solve :: [[Int]] -> Maybe [[Int]]
solve lst =
  case emptyPos of
  [] -> Just lst -- no empty place, meaning the puzzle is solved
  (firstEmptyPos:_) -> firstSomething $
        map (solve . setValue lst firstEmptyPos) possibleNewValues
    where possibleNewValues = possible lst firstEmptyPos
          -- if possibleNewValues is empty, then solve returns Nothing
  where emptyPos = emptyPositions lst


printNicely :: Maybe [[Int]] -> IO ()
printNicely Nothing = putStrLn "No solution."
printNicely (Just lst) = forM_ lst print
