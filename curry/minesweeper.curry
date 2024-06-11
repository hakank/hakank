{- 
  
  Minsweeper in Curry

  From gecode/examples/minesweeper.cc:
  """
  A specification is a square matrix of characters. Alphanumeric characters represent
  the number of mines adjacent to that field. Dots represent fields with an unknown number
  of mines adjacent to it (or an actual mine).
  """
  
  E.g.
       "..2.3."
       "2....."
       "..24.3"
       "1.34.."
       ".....3"
       ".3.3.."
  """
  
  For more examples, see my Picat model http://hakank.org/picat/minesweeper.pi

  Also see 
   
  http://www.janko.at/Raetsel/Minesweeper/index.htm

  http://en.wikipedia.org/wiki/Minesweeper_(computer_game)

  Ian Stewart on Minesweeper: http://www.claymath.org/Popular_Lectures/Minesweeper/

  Richard Kaye's Minesweeper Pages
  http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.htm
  Some Minesweeper Configurations
  http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.pdf

  

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (chunksOf,matrixElement)
import CLP.FD
import Debug.Trace


--
-- Constraints
-- If a cell in the problem matrix contains a value (>= 0) then
--  a) the value is the number of surrounding bombs
--  b) this cell cannot be a bomb (i.e. it must be 0)
-- Otherwise:
--  This cell might be a bomb
--
check :: [[Int]] -> [[CLP.FD.FDExpr]] -> Int -> Int -> (Int, Int) -> CLP.FD.FDConstr
check p x rows cols (i,j) = if matrixElement p i j > u then
                               -- if the problem matrix has a hint then that value is
                               -- the number of bombs surrounding it

                               -- Using CLP.FD.sum and Equ
                               -- CLP.FD.sum [matrixElement x (i+a) (j+b)|
                               --             a <- [(-1)..1], b <- [(-1)..1],
                               --             i+a >= 0, i+a < rows, j+b >= 0, j+b < cols] Equ (fd (matrixElement p i j ))
                               
                               -- This works as well i.e. using =# and Data.List.sum
                               -- I find this a little more natural than using CLP.FD.Sum and Equ
                               -- since we can have the list/value on either side of =# .
                               fd (matrixElement p i j) =# Data.List.sum [matrixElement x (i+a) (j+b) |
                                                                     a <- [(-1)..1], b <- [(-1)..1],
                                                                     i+a >= 0, i+a < rows, j+b >= 0, j+b < cols]

                               /\ -- This cell cannot be a bomb
                               matrixElement x i j =# (fd 0)
                            else
                               true

minesweeper' :: [[Int]] -> Int -> Int -> [[Int]]
minesweeper' p rows cols = let
                   x' = take (rows*cols) (domain 0 1) -- This is a plain list
                   x = chunksOf cols x'               -- Convert to a matrix to simplify access of the indices
                in
                   solveFDAll [FirstFail] x' $
                   -- We only need to check the cells which contains a number >= 0
                   foldl1 (/\) [ check p x rows cols (i,j) | i <- [0..rows-1], j <- [0..cols-1], matrixElement p i j > u ]
                   

minesweeper :: [[Prelude.Int]] -> [[[Prelude.Int]]]
minesweeper p = map (\s -> chunksOf cols s) $ minesweeper' p rows cols
                where
                   rows = length p
                   cols = length $ head p


--
-- Print all solutions for instance p
--
printSol :: Prelude.Show a => [a] -> Prelude.IO ()
printSol p = do
                (mapM_ print p)
                putStrLn "\n"

printSols :: [[Prelude.Int]] -> Prelude.IO ()
printSols p = mapM_ ( printSol ) $ minesweeper p

--
-- Unknown cell
--
u :: Num a => a
u = (-1)


--
-- Data
-- 

--
-- Problem from Gecode/examples/minesweeper.cc  problem 0
--
problem0 :: Num a => [[a]]
problem0 = [[u,u,2,u,3,u],
            [2,u,u,u,u,u],
            [u,u,2,4,u,3],
            [1,u,3,4,u,u],
            [u,u,u,u,u,3],
            [u,3,u,3,u,u]]


--
-- Problem from Gecode/examples/minesweeper.cc  problem 1
--
problem1 :: Num a => [[a]]
problem1 = [[u,2,u,2,1,1,u,u],
            [u,u,4,u,2,u,u,2],
            [2,u,u,2,u,u,3,u],
            [2,u,2,2,u,3,u,3],
            [u,u,1,u,u,u,4,u],
            [1,u,u,u,2,u,u,3],
            [u,2,u,2,2,u,3,u],
            [1,u,1,u,u,1,u,1]]

--
-- Problem from Gecode/examples/minesweeper.cc  problem 9
--
problem9 :: Num a => [[a]]
problem9 =  [[2,u,u,u,2,u,u,u,2],
             [u,4,u,4,u,3,u,4,u],
             [u,u,4,u,u,u,1,u,u],
             [u,4,u,3,u,3,u,4,u],
             [2,u,u,u,u,u,u,u,2],
             [u,5,u,4,u,5,u,4,u],
             [u,u,3,u,u,u,3,u,u],
             [u,4,u,3,u,5,u,6,u],
             [2,u,u,u,1,u,u,u,2]]



--
-- Richard Kaye: How Complicated is Minesweeper?
-- http://web.mat.bham.ac.uk/R.W.Kaye/minesw/ASE2003.pdf
-- 
-- A Wire, page 33
-- 2 solutions
--
problem12 :: Num a => [[a]]
problem12 = [[u,0,0,0,0,0,0,0,0,0,0,0,0,u],
             [u,1,1,1,1,1,1,1,1,1,1,1,1,u],
             [u,u,1,u,u,1,u,u,1,u,u,1,u,u],
             [u,1,1,1,1,1,1,1,1,1,1,1,1,u],
             [u,0,0,0,0,0,0,0,0,0,0,0,0,u]]

-- Just testing
problemx :: Num a => [[a]]
problemx = [[0,u,u,1],
            [u,u,u,u],
            [0,u,u,1]]

--
-- Tests
--

{-
  [1,0,0,0,0,1]
  [0,1,0,1,1,0]
  [0,0,0,0,1,0]
  [0,0,0,0,1,0]
  [0,1,1,1,0,0]
  [1,0,0,0,1,1]

  Execution time: 24 msec. / elapsed: 33 msec.
-}
main :: IO ()
main = printSols problem0

{-
  [0,0,1,0,0,0,1,0]
  [1,0,0,1,0,0,0,0]
  [0,1,1,0,0,1,0,1]
  [0,0,0,0,0,0,1,0]
  [1,0,0,0,1,0,0,1]
  [0,0,1,0,0,1,1,0]
  [0,0,0,0,0,0,0,1]
  [0,1,0,0,1,0,0,0]

  Execution time: 52 msec. / elapsed: 55 msec.

-}
main2 :: IO ()
main2 = printSols problem1

{-

  [0,1,0,1,0,0,1,1,0]
  [1,0,1,0,1,0,0,0,1]
  [0,1,0,1,0,1,0,0,1]
  [0,0,1,0,0,0,0,0,1]
  [0,1,1,0,0,1,1,1,0]
  [1,0,1,0,1,0,0,0,0]
  [0,1,0,0,1,1,0,1,1]
  [1,0,1,0,1,0,1,0,1]
  [0,1,0,0,0,0,1,1,0]

  Execution time: 66 msec. / elapsed: 69 msec.

-}
main3 :: IO ()
main3 = printSols problem9


{-
  [0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  [0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  [0,1,0,0,1,0,0,1,0,0,1,0,0,1]
  [0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  [0,0,0,0,0,0,0,0,0,0,0,0,0,0]


  [0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  [0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  [1,0,0,1,0,0,1,0,0,1,0,0,1,0]
  [0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  [0,0,0,0,0,0,0,0,0,0,0,0,0,0]

  Execution time: 58 msec. / elapsed: 72 msec.

-}
main4 :: IO ()
main4 = printSols problem12


main5 :: IO ()
main5 = printSols problemx
