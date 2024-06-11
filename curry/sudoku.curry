{- 
  
  Sudoku in Curry CLP.FD

  This is a port of my Picat model http://hakank.org/picat/sudoku.pi

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (chunksOf,matrixElement)
import CLP.FD

--
-- Enforce the hints from p (i.e. the one that's > 0) to the decision variable matrix x
--
addHints :: [[Int]] -> [[CLP.FD.FDExpr]] -> CLP.FD.FDConstr
addHints p x = foldl1 (/\) [ matrixElement x i j =# fd (matrixElement p i j)  |
                             let n = length p, i <- [0..n-1], j <- [0..n-1],
                             matrixElement p i j > 0 ] 

--
-- Solve a Sudoku instance
-- 
sudoku :: [[Int]] -> [Int]
sudoku p = let
             n = length p
             s = floor $ sqrt (fromIntegral n)
             x' = take (n*n) (domain 1 n)
             x = chunksOf n x'
           in
             solveFD[FirstFail,Bisect] x' $
             addHints p x /\
             andC [ allDifferent row | row <- x] /\
             andC [ allDifferent col | col <- transpose x] /\
             andC [ allDifferent [ matrixElement x (i+k) (j+l) | k <- [0..s-1], l <- [0..s-1]]
                           | i <- [0,s..n-1], j <- [0,s..n-1]]


printSolution :: [[Int]] -> IO ()
printSolution p = mapM_ print $ chunksOf n $ sudoku p
                  where
                     n = length p


main1 :: IO ()
main1 = printSolution problem_p1

main2 :: IO ()
main2 = printSolution problem_hardest_ever

main3 :: IO ()
main3 = printSolution problem_34

main4 :: IO ()
main4 = printSolution problem_89

main :: IO ()
main = do
          putStrLn "problem 1:"
          main1
          
          putStrLn "\nproblem hardest_ever:"
          main2
          
          putStrLn "\nproblem 34:"          
          main3

          -- putStrLn "\nproblem 34:"          
          -- main4 -- too slow

--
-- Some problem instances from http://hakank.org/picat/sudoku.pi
--

{-
  This problem is problem 1 from
  Gecode's sudoku.cpp
  http://www.gecode.org/gecode-doc-latest/sudoku_8cpp-source.html

  [3,6,2,8,4,5,1,7,9]
  [1,7,5,9,6,3,2,4,8]
  [9,4,8,2,1,7,6,3,5]
  [7,1,3,4,5,8,9,6,2]
  [2,9,6,7,3,1,5,8,4]
  [8,5,4,6,2,9,7,1,3]
  [4,3,9,5,7,6,8,2,1]
  [5,2,7,1,8,4,3,9,6]
  [6,8,1,3,9,2,4,5,7]
  Execution time: 16 msec. / elapsed: 24 msec.

-}
problem_p1 :: [[Int]]
problem_p1 = [[0, 0, 2, 0, 0, 5, 0, 7, 9],
              [1, 0, 5, 0, 0, 3, 0, 0, 0],
              [0, 0, 0, 0, 0, 0, 6, 0, 0],
              [0, 1, 0, 4, 0, 0, 9, 0, 0],
              [0, 9, 0, 0, 0, 0, 0, 8, 0],
              [0, 0, 4, 0, 0, 9, 0, 1, 0],
              [0, 0, 9, 0, 0, 0, 0, 0, 0],
              [0, 0, 0, 1, 0, 0, 3, 0, 6],
              [6, 8, 0, 3, 0, 0, 4, 0, 0]]

{-
 
  From
  http://www.kristanix.com/sudokuepic/worlds-hardest-sudoku.php
  """
  For those of us that never tire of a well made sudoku challenge, 
  Finnish mathematician, Arto Inkala has made what he claims is the 
  hardest sudoku puzzle ever. According to the Finnish puzzle maker 
  "I called the puzzle AI Escargot, because it looks like a snail. 
  Solving it is like an intellectual culinary pleasure. AI are my 
  initials".
  
  If you're open for the challenge, AI Escargot presumably requires 
  you to wrap your brain around eight casual relationships 
  simultaneously, whereas your everyday "very hard" sudoku piece, 
  only require you to think about a meager one or two of these 
  relationships at once.
  """
 
  [1,6,2,8,5,7,4,9,3]
  [5,3,4,1,2,9,6,7,8]
  [7,8,9,6,4,3,5,2,1]
  [4,7,5,3,1,2,9,8,6]
  [9,1,3,5,8,6,7,4,2]
  [6,2,8,7,9,4,1,3,5]
  [3,5,6,4,7,8,2,1,9]
  [2,4,1,9,3,5,8,6,7]
  [8,9,7,2,6,1,3,5,4]
  Execution time: 20 msec. / elapsed: 20 msec.
 
-}
problem_hardest_ever :: [[Int]]
problem_hardest_ever = [[1,0,0, 0,0,7, 0,9,0],
                        [0,3,0, 0,2,0, 0,0,8],
                        [0,0,9, 6,0,0, 5,0,0],

                        [0,0,5, 3,0,0, 9,0,0],
                        [0,1,0, 0,8,0, 0,0,2],
                        [6,0,0, 0,0,4, 0,0,0],

                        [3,0,0, 0,0,0, 0,1,0],
                        [0,4,0, 0,0,0, 0,0,7],
                        [0,0,7, 0,0,0, 3,0,0]]



{-
  This problem is problem 34 from
  Gecode's sudoku.cpp
  http://www.gecode.org/gecode-doc-latest/sudoku_8cpp-source.html
 
  Size : 16 x 16
 
  [13,9,2,11,15,12,10,1,16,6,14,7,4,3,8,5]
  [4,12,15,10,3,5,16,8,9,13,1,2,7,6,14,11]
  [3,14,7,1,4,6,2,13,15,5,8,11,12,9,16,10]
  [16,5,6,8,9,7,14,11,10,3,12,4,15,13,2,1]
  [12,7,16,5,10,8,11,15,3,2,6,1,14,4,9,13]
  [2,13,8,4,12,3,1,14,5,11,7,9,10,15,6,16]
  [1,11,10,14,2,9,6,4,13,15,16,8,3,7,5,12]
  [6,15,9,3,5,13,7,16,4,12,10,14,11,2,1,8]
  [15,3,14,16,1,2,9,5,7,8,11,10,6,12,13,4]
  [9,10,11,12,16,15,8,7,6,4,5,13,2,1,3,14]
  [5,8,4,2,13,10,3,6,14,1,9,12,16,11,7,15]
  [7,6,1,13,11,14,4,12,2,16,3,15,5,8,10,9]
  [11,4,12,9,7,16,5,2,8,10,13,3,1,14,15,6]
  [8,2,5,6,14,1,15,9,12,7,4,16,13,10,11,3]
  [14,1,3,15,6,4,13,10,11,9,2,5,8,16,12,7]
  [10,16,13,7,8,11,12,3,1,14,15,6,9,5,4,2]
  Execution time: 1560 msec. / elapsed: 1570 msec.
 
-} 
problem_34 :: [[Int]]
problem_34 =  [[13, 9, 2, 0, 0, 0, 0, 0,16, 0, 0, 0, 4, 3, 0, 0],
               [ 4,12,15, 0, 0, 0, 0, 0, 9,13, 0, 2, 0, 6,14,11],
               [ 0,14, 0, 1, 0, 0, 0, 0,15, 0, 8,11,12, 0, 0,10],
               [16, 5, 6, 0, 0, 0, 0, 0,10, 3,12, 0, 0, 0, 0, 1],
               [ 0, 7,16, 5,10, 8, 0, 0, 0, 0, 6, 1, 0, 0, 0, 0],
               [ 2, 0, 0, 0,12, 0, 0, 0, 0,11, 7, 0, 0, 0, 0, 0],
               [ 0, 0,10,14, 0, 9, 6, 4, 0, 0,16, 0, 0, 0, 0, 0],
               [ 0,15, 9, 0, 5, 0, 7, 0, 4, 0, 0, 0, 0, 0, 0, 0],
               [ 0, 0, 0, 0, 0, 2, 9, 0, 0, 0, 0,10, 0,12, 0, 0],
               [ 0, 0, 0, 0, 0, 0, 0, 0, 6, 4, 5,13, 0, 1, 0, 0],
               [ 0, 0, 0, 0,13, 0, 0, 0, 0, 1, 0,12, 0,11, 7,15],
               [ 0, 0, 0, 0, 0,14, 0,12, 2,16, 0, 0, 0, 8,10, 9],
               [11, 0, 0, 9, 0,16, 5, 2, 0, 0, 0, 0, 0,14,15, 6],
               [ 0, 2, 5, 6, 0, 0,15, 0, 0, 0, 0, 0,13, 0,11, 0],
               [14, 1, 3, 0, 6, 0,13, 0, 0, 0, 0, 0, 0, 0, 0, 7],
               [10, 0, 0, 0, 8,11,12, 3, 0, 0, 0, 0, 9, 5, 4, 0]]



-- 
-- This problem is problem 89 from
-- Gecode's sudoku.cpp
-- http://www.gecode.org/gecode-doc-latest/sudoku_8cpp-source.html
--
-- Size : 25 x 25
--
--
-- This takes > 30min
--
problem_89 :: [[Int]]
problem_89 = [[11,23,13,10,19,16, 6, 2,24, 7, 5, 9, 1,20,17,15, 8,18,25, 3, 4,12,21,22,14],
              [15,16, 0,22, 0,11, 8, 0, 0, 0,25, 0,14, 0, 0, 0,12,19, 0, 0,17, 0, 0, 0, 0],
              [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,16, 0, 4, 0,17, 0,13, 0,24, 0,23,19,10, 2],
              [ 0, 0, 0, 0, 0,19, 0,14,23, 4, 0,21, 6,22,10, 0,11, 0, 2, 0, 0, 0, 0, 0, 0],
              [17,14, 0, 0, 2, 0, 0,13,12, 0, 0, 0, 0, 0,15, 4,20,22,10, 0,11, 0, 9,24, 8],
              [22, 0, 0, 0, 0, 6, 2, 0, 0, 0, 4, 7,12, 1, 9, 0, 0, 0, 0, 0, 0,14, 5, 0, 0],
              [ 0,18, 2, 0, 8,22, 0,19,16,21, 0, 0, 0,10,13,23, 0, 0,20, 0, 0, 3, 0,15, 7],
              [ 0, 0,17, 3, 0, 5, 0, 0, 8, 9, 0, 0, 0, 0,18, 0,19, 0, 0, 0, 0, 0,23,21, 0],
              [ 1,11, 0, 0, 9, 0,15,10,25, 0, 6, 0,23, 0, 0, 0, 0, 5, 3, 7, 0,17, 0, 0,24],
              [ 0, 0, 0, 0, 0, 0, 1, 0, 0,23, 0, 0, 0,24, 0, 0, 0,21,12, 0, 6, 8, 0,25,16],
              [20,24,10, 0,15,23,11,17, 0, 0, 0, 0, 0, 7, 0,12, 0, 0, 0, 0, 0,22, 0, 0, 6],
              [ 4, 5, 0,14,12,25, 0,18, 0, 0,23, 0,15, 0,19, 1, 0, 0, 0,22,20, 0, 7, 9, 0],
              [18, 0,21, 0, 0, 8, 0,24, 0, 0, 9, 0,25, 0, 0, 0,10, 0, 0, 0, 2, 0, 1,19, 0],
              [ 0, 0, 6, 2, 1, 0,13, 0,22, 0, 0, 0, 0, 0,11, 8,21,16, 0, 0,25, 0, 0,12,17],
              [ 0,17,25, 0,23, 7,14, 0,21, 1, 0, 0, 0, 0, 3, 0, 0,11, 0, 0,24, 0,16, 4, 5],
              [ 0, 0, 0, 0,11,18,24, 0, 0, 0, 0, 5, 0,12, 0,25, 0, 0, 0,15,23, 4, 8,14, 0],
              [ 0, 0, 0,15,21, 0, 0, 0, 0, 0, 2, 0,13,17, 0, 0, 1, 7, 0, 0, 5, 9,24, 0, 0],
              [ 0, 0,18, 0,22,15, 0, 0, 2,16, 0,23, 0, 0, 0,10, 6,24, 0,17,12, 0,25,11, 0],
              [ 7, 2, 0, 1, 0, 0,21, 0, 0, 0,18,22, 0, 9, 6,14, 0, 4, 5,16, 0, 0, 0, 0, 0],
              [ 0, 0, 9, 0, 0, 0, 7,22, 0, 0,10, 0,24, 0, 0, 0,18, 0, 0, 0,21, 0, 0, 0, 0],
              [ 0,12, 0,19,10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,14, 0, 4, 8, 0],
              [24, 0,11,18, 0, 0, 0, 0, 0, 0, 0,25,17,21, 0, 6, 0, 0, 1, 0, 0, 0, 0, 5,12],
              [16, 6,22, 0, 0, 0,23, 4,15,18, 8, 0, 0, 0,20, 0, 0,17, 0,14, 0, 0, 0, 0, 0],
              [ 0,21, 0, 0, 4, 0, 9, 1, 7, 0, 0, 0, 0,11,14, 0,16, 8,15, 0,22, 0,18, 0, 0],
              [ 8,15, 0, 0, 0, 0, 0, 0, 5, 0,24, 3, 0, 0, 4, 0, 0, 0, 9, 0, 0, 0, 0, 0,20]]






