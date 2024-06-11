{- 
  
  Survo puzzle in Curry CLP.FD

  http://en.wikipedia.org/wiki/Survo_Puzzle
  """
  Survo puzzle is a kind of logic puzzle presented (in April 2006) and studied 
  by Seppo Mustonen. The name of the puzzle is associated to Mustonen's 
  Survo system which is a general environment for statistical computing and 
  related areas.
  
  In a Survo puzzle the task is to fill an m * n table by integers 1,2,...,m*n so 
  that each of these numbers appears only once and their row and column sums are 
  equal to integers given on the bottom and the right side of the table. 
  Often some of the integers are given readily in the table in order to 
  guarantee uniqueness of the solution and/or for making the task easier.
  """
  
  See also
  http://www.survo.fi/english/index.html
  http://www.survo.fi/puzzles/index.html
 
  References:
  Mustonen, S. (2006b). "On certain cross sum puzzles"
  http://www.survo.fi/papers/puzzles.pdf 
  Mustonen, S. (2007b). "Enumeration of uniquely solvable open Survo puzzles." 
  http://www.survo.fi/papers/enum_survo_puzzles.pdf 
  Kimmo Vehkalahti: "Some comments on magic squares and Survo puzzles" 
  http://www.helsinki.fi/~kvehkala/Kimmo_Vehkalahti_Windsor.pdf
  R code: http://koti.mbnet.fi/tuimala/tiedostot/survo.R


  This is a port of my Picat model http://hakank.org/picat/survo_puzzle.pi


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (chunksOf)
import CLP.FD


survoPuzzle :: ([Int], [Int], [[Int]]) -> [Int]
survoPuzzle p = let
                  (rowsums,colsums,problem) = p
                  rows = length rowsums
                  cols = length colsums
                  rc = rows*cols
                  x' = take rc (domain 1 rc)
                  x  = chunksOf cols x'
               in
                 solveFD [] x' $
                 allDifferent x' /\
                 -- The given hints in the problem grid
                 foldl1 (/\)  [ if a > 0 then v =# (fd a) else true | (a,v) <- zip (concat problem) x'] /\
                 -- row sums
                 foldl1 (/\) [ Data.List.sum row =# (fd s) |  (row,s) <- zip x rowsums] /\
                 -- col sums
                 foldl1 (/\) [ Data.List.sum col =# (fd s) |  (col,s) <- zip (transpose x) colsums]


printSolution :: ([Int], [Int], [[Int]]) -> IO ()
printSolution p =  mapM_ print $ chunksOf (numCols p) $ survoPuzzle p

numCols :: (a,[Int],b) -> Int
numCols (_,colsums,_) = length colsums

main1 :: IO ()
main1 = printSolution problem1

main2 :: IO ()
main2 = printSolution problem2

main3 :: IO ()
main3 = printSolution problem3

main4 :: IO ()
main4 = printSolution problem4

main5 :: IO ()
main5 = printSolution problem5

main6 :: IO ()
main6 = printSolution problem6

main7 :: [[Int]]
main7 =  p =:= anyOf problems &> chunksOf (numCols p) $ survoPuzzle p  where p free

main :: IO ()
main = do
          putStrLn "problem1:"
          main1
          putStrLn "\nProblem 2:"
          main2
          putStrLn "\nProblem3:"
          main3
          putStrLn "\nProblem4:"
          main4
          putStrLn "\nProblem5:"
          main5
          putStrLn "\nProblem6:"
          main6
          
--
-- Data
--
problems :: [([Int], [Int], [[Int]])]
problems = [problem1,problem2,problem3,problem4,problem5,problem6]

--
-- http://en.wikipedia.org/wiki/Survo_Puzzle, first example
--
-- Solution:
--  12 6 2 10
--  8 1 5 4
--  7 9 3 11
--
problem1 :: ([Int], [Int], [[Int]])
problem1 = ([30,18,30],    -- Row sums
            [27,16,10,25], -- Col sums
            [[0, 6, 0, 0], -- Problem grid
             [8, 0, 0, 0],
             [0, 0, 3, 0]])

--
-- http://en.wikipedia.org/wiki/Survo_Puzzle, second example
-- difficulty 0
--
problem2 :: ([Int], [Int], [[Int]])
problem2 = ([9, 12],     -- rowsums
            [9, 7, 5],   -- colsums
            [[0, 0, 3],  -- problem
             [0, 6, 0]])
        

--
-- http://en.wikipedia.org/wiki/Survo0Puzzle, third example
-- difficulty 150 ("open puzzle", i.e. no hints]
-- It's an unique solution.
-- (817 propagations with Gecode/fz, and 33 failures, 88 commits]
-- r = 3;
-- c = 4;
-- rowsums = [24,15,39];
-- colsums = [21,10,18,29];
-- matrix = array2d(1..r, 1..c, 
--   [
--     0, 0, 0, 0,
--     0, 0, 0, 0,
--     0, 0, 0, 0
--   ]];
-- Note: this version has no hints
--
problem3 :: ([Int], [Int], [[Int]])
problem3 = ([24,15,39],    -- rowsums
            [21,10,18,29], -- colsums
            [[0, 0, 0, 0], -- problem
             [0, 0, 0, 0],
             [0, 0, 0, 0]])



--
-- Same as above but with hints: difficulty 0
--
problem4 :: ([Int], [Int], [[Int]])
problem4 = ([24,15,39],    -- rowsums
            [21,10,18,29], -- colsums
            [[7, 0, 5, 0], -- problem
             [0, 1, 0, 8],
             [0, 0, 11, 0]])


--
-- http://www.survo.fi/puzzles/280708.txt, third puzzle
-- Survo puzzle 128/2008 (1700] #364-35846
--
--    A  B  C  D  E  F
-- 1  *  *  *  *  *  * 30
-- 2  *  * 18  *  *  * 86
-- 3  *  *  *  *  *  * 55
--   22 11 42 32 27 37
--
problem5 :: ([Int], [Int], [[Int]])
problem5 = ([30, 86, 55],
            [22, 11, 42, 32, 27, 37],
            [[0, 0,  0, 0, 0, 0],
             [0, 0, 18, 0, 0, 0],
             [0, 0,  0, 0, 0, 0]])

--
-- http://en.wikipedia.org/wiki/Survo0Puzzle, under "Swapping method"
-- (open puzzle]
--
problem6 :: ([Int], [Int], [[Int]])
problem6 = ([51,36,32,17],
            [51,42,26,17],
            [[0, 0, 0, 0],
             [0, 0, 0, 0],
             [0, 0, 0, 0],
             [0, 0, 0, 0]])

