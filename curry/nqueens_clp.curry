{- 
  
  N-queens problem in Curry CLP.FD

  https://en.wikipedia.org/wiki/Eight_queens_puzzle
  ""
  The eight queens puzzle is the problem of placing eight chess queens on an 8×8 chessboard so 
  that no two queens threaten each other; thus, a solution requires that no two queens share the 
  same row, column, or diagonal. There are 92 solutions.
  """

  This is a port of my Picat model http://hakank.org/picat/queens.pi

  Note: using the allDifferent constraint for the two 'other' constraints 
  yield TODO: Show CLP.FD.FDExpr.

  However, using a decomposition (HakankUtilsCLPFD.allDifferent2) works.

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import HakankUtilsCLPFD (allDifferent2)
import CLP.FD

nqueens :: Int -> [Int]
nqueens n =  let
                x = take n (domain 1 n)
             in
                solveFD [FirstFail,Bisect] x $
                allDifferent x /\
                -- These constraints yield ERROR: TODO: Show CLP.FD.FDExpr
                -- allDifferent [ (x !! (i-1) ) - (fd i) | i <- [1..n-1]] /\
                -- allDifferent [ (x !! (i-1) ) + (fd i) | i <- [1..n-1]]

                -- But this works!
                allDifferent2 [ (x !! (i-1)) - (fd i) | i <- [1..n]] /\
                allDifferent2 [ (x !! (i-1)) + (fd i) | i <- [1..n]]

{-

  All solutions for 5 queens:

  [1,3,5,2,4]
  [1,4,2,5,3]
  [2,4,1,3,5]
  [2,5,3,1,4]
  [3,1,4,2,5]
  [3,5,2,4,1]
  [4,1,3,5,2]
  [4,2,5,3,1]
  [5,2,4,1,3]
  [5,3,1,4,2]
  Execution time: 45 msec. / elapsed: 45 msec.

-}
main :: IO ()
main = mapM_ print $ allValues $ nqueens 5

{-
  Some of the 92 solutions for 8-queens:

  [1,5,8,6,3,7,2,4]
  [1,6,8,3,7,4,2,5]
  [1,7,4,6,8,2,5,3]
  [1,7,5,8,2,4,6,3]
  [2,4,6,8,3,1,7,5]
  [2,5,7,1,3,8,6,4]
  [2,5,7,4,1,8,6,3]
  ...
  [7,3,8,2,5,1,6,4]
  [7,4,2,5,8,1,3,6]
  [7,4,2,8,6,1,3,5]
  [7,5,3,1,6,8,2,4]
  [8,2,4,1,7,5,3,6]
  [8,2,5,3,1,7,4,6]
  [8,3,1,6,2,5,7,4]
  [8,4,1,3,6,2,7,5]

  Execution time: 68 msec. / elapsed: 75 msec.

-}
main2 :: IO ()
main2 = mapM_ print $ allValues $ nqueens 8


{-

  The number of solutions for n=2..13

  (2,0)
  (3,0)
  (4,2)
  (5,10)
  (6,4)
  (7,40)
  (8,92)
  (9,352)
  (10,724)
  (11,2680)
  (12,14200)
  (13,73712)

  Execution time: 12532 msec. / elapsed: 12549 msec.

-}
main3 :: IO ()
main3 = mapM_ print $ map (\n -> (n,length . allValues $ nqueens n)) [2..13]

{-
  One solution for n = 100:

  Just [1,3,5,57,59,4,64,7,58,71,81,60,6,91,82,90,8,83,77,65,73,26,9,45,37,63,66,62,44,10,48,54,43,69,42,47,18,11,72,68,50,56,61,36,33,17,12,51,100,93,97,88,35,84,78,19,13,99,67,76,92,75,87,96,94,85,20,14,95,32,98,55,40,80,49,52,46,53,21,15,41,2,27,34,22,70,74,29,25,30,38,86,16,79,24,39,28,23,31,89]
  Execution time: 7192 msec. / elapsed: 10434 msec.

-}
main4 :: Maybe [Int]
main4 = oneValue $ nqueens 100