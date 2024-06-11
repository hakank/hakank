{- 
  
  Coins grid problem in Curry

  Problem from 
  Tony Hürlimann: "A coin puzzle - SVOR-contest 2007"
  http://www.svor.ch/competitions/competition2007/AsroContestSolution.pdf
  """
  In a quadratic grid (or a larger chessboard) with 31x31 cells, one 
  should place coins in such a way that the following conditions are 
  fulfilled:
    1. In each row exactly 14 coins must be placed.
    2. In each column exactly 14 coins must be placed.
    3. The sum of the quadratic horizontal distance from the main
       diagonal of all cells containing a coin must be as small as possible.
    4. In each cell at most one coin can be placed.

   The description says to place 14x31 = 434 coins on the chessboard 
   each row containing 14 coins and each column also containing 14 coins.
  """

  Note: This problem is quite hard for CP solvers. A MIP solver solves
  the 14,31 problem in millis.

  TODO: How do I minimize s? 
  The call
    solveFDAll [Minimize s] ... 
  does not work

  The brute force of sorting all the solutions works for very small problems though.


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (chunksOf,matrixElement)
import CLP.FD


coinsGrid n c = let
                  x' = take (n*n) (domain 0 1)
                  x = chunksOf n x'
                  s = head (domain 0 (n*n*n*n))
                in
                  solveFDAll [] (s:x') $
                  Data.List.sum [(matrixElement x i j)*(fd (a*a)) | i <- [0..n-1], j <- [0..n-1], let a = abs(i-j), a > 0] =# s /\
                  foldl1 (/\) [ Data.List.sum row =# fd c | row <- x] /\
                  foldl1 (/\) [ Data.List.sum row =# fd c | row <- (transpose x)]                  



showSol n c =  do
           let sols = coinsGrid n c
           putStrLn ("numSols: " ++ (show (length sols)))           
           let (opt:sol) = head . sort $ sols
           putStrLn ("opt: " ++ (show opt))
           mapM_ print $ chunksOf n sol

{-

  numSols: 2040
  opt: 26
  [1,1,1,0,0]
  [1,1,0,1,0]
  [1,1,0,0,1]
  [0,0,1,1,1]
  [0,0,1,1,1]

-}
main = showSol 5 3