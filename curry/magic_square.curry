{- 
  
  Magic square in Curry CLP.FD

  https://en.wikipedia.org/wiki/Magic_square
  """
  In recreational mathematics, a square array of numbers, usually positive integers, 
  is called a magic square if the sums of the numbers in each row, each column, 
  and both main diagonals are the same.[1][2] The 'order' of the magic square 
  is the number of integers along one side (n), and the constant sum is called the 
  'magic constant'. If the array includes just the positive integers 
  
  """

  This is a port of my Picat model http://hakank.org/picat/magic_square.pi

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (chunksOf,matrixElement)
import CLP.FD


magicSquare :: Int -> [[Int]]
magicSquare n = let
                  nn = n*n
                  x' = take (nn) (domain 1 nn)
                  x  = chunksOf n x'
                  s  = fd $ n*(nn+1) `div` 2 
                in
                  solveFDAll [FirstFail,Bisect] x' $
                  allDifferent x' /\
                  foldl1 (/\) (map (\row -> CLP.FD.sum row Equ s) x) /\
                  foldl1 (/\) (map (\row -> CLP.FD.sum row Equ s) (transpose x)) /\
                  CLP.FD.sum [matrixElement x i i | i <- [0..n-1]] Equ s /\
                  CLP.FD.sum [matrixElement x i (n-i-1) | i <- [0..n-1]] Equ s

--
-- For first solutions (using solveFD instead of solveFDAll)
--
magicSquare2 :: Int -> [Int]
magicSquare2 n = let
                  nn = n*n
                  x' = take (nn) (domain 1 nn)
                  x  = chunksOf n x'
                  s  = fd $ n*(nn+1) `div` 2 
                in
                  solveFD [FirstFail,Bisect] x' $
                  allDifferent x' /\
                  foldl1 (/\) (map (\row -> CLP.FD.sum row Equ s) x) /\
                  foldl1 (/\) (map (\row -> CLP.FD.sum row Equ s) (transpose x)) /\
                  CLP.FD.sum [matrixElement x i i | i <- [0..n-1]] Equ s /\
                  CLP.FD.sum [matrixElement x i (n-i-1) | i <- [0..n-1]] Equ s

printSol :: Show a => [a] -> IO ()
printSol p = do
                (mapM_ print p)
                putStrLn ""

printSols :: Int -> IO ()
printSols n = mapM_ ( printSol ) $ map (chunksOf n) $ magicSquare n

{-
  All 8 solutions for n=3

  [2,7,6]
  [9,5,1]
  [4,3,8]

  [2,9,4]
  [7,5,3]
  [6,1,8]

  [4,3,8]
  [9,5,1]
  [2,7,6]

  [4,9,2]
  [3,5,7]
  [8,1,6]

  [6,1,8]
  [7,5,3]
  [2,9,4]

  [6,7,2]
  [1,5,9]
  [8,3,4]

  [8,1,6]
  [3,5,7]
  [4,9,2]

  [8,3,4]
  [1,5,9]
  [6,7,2]

  Execution time: 10 msec. / elapsed: 10 msec.

-}
main :: IO ()
main = printSols 3

{-
  Number of solutions for n=1..4:
  (1,1)
  (2,0)
  (3,8)
  (4,7040)

  Execution time: 2504 msec. / elapsed: 2522 msec.

-}
main2 :: IO ()
main2 = mapM_ (\n -> print (n,length $ magicSquare n)) [1..4]

{-
  All 7040 solutions for n=4

  ...

  [16,14,3,1]
  [7,4,13,10]
  [2,11,6,15]
  [9,5,12,8]

  [16,15,1,2]
  [4,3,13,14]
  [5,10,8,11]
  [9,6,12,7]

  [16,15,2,1]
  [4,3,14,13]
  [5,10,7,12]
  [9,6,11,8]

  [16,15,2,1]
  [5,3,14,12]
  [4,10,7,13]
  [9,6,11,8]

  Execution time: 7567 msec. / elapsed: 9214 msec.

-}
main3 :: IO ()
main3 = printSols 4


{-
  Just [[1,2,3,34,35],[36,4,18,28,29],[5,27,10,26,30],[8,23,14,31,25],[13,21,15,6,32],[16,20,12,22,9],[33,24,17,7,11]]
  Execution time: 597 msec. / elapsed: 601 msec.

  i.e. 
  [ 1, 2, 3,34,35]
  [36, 4,18,28,29]
  [ 5,27,10,26,30]
  [ 8,23,14,31,25]
  [13,21,15, 6,32]
  [16,20,12,22, 9]
  [33,24,17,7,11]]

-}
main4 = fmap (chunksOf 5) $ oneValue $ magicSquare2 6