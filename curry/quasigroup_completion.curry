{- 
  
  Quasigroup completion in Curry

  See 
  Carla P. Gomes and David Shmoys:
  "Completing Quasigroups or Latin Squares: Structured Graph Coloring Problem"

  
  See also
  Ivars Peterson "Completing Latin Squares"
  http://www.maa.org/mathland/mathtrek_5_8_00.html
  """
  Using only the numbers 1, 2, 3, and 4, arrange four sets of these 
  numbers into a four-by-four array so that no column or row contains 
  the same two numbers. The result is known as a Latin square.
  ...
  The so-called quasigroup completion problem concerns a table that is 
  correctly but only partially filled in. The question is whether the 
  remaining blanks in the table can be filled in to obtain a complete 
  Latin square (or a proper quasigroup multiplication table).
  """

  This is a port of my Picat model http://hakank.org/picat/quasigroup_completion.pi

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (chunksOf)
-- import HakankUtilsCLPD 
import CLP.FD

latinSquare' :: [[CLP.FD.FDExpr]] -> CLP.FD.FDConstr
latinSquare' [] = true
latinSquare' (x:xs) = allDifferent x /\ latinSquare' xs

latinSquare :: [[CLP.FD.FDExpr]] -> CLP.FD.FDConstr
latinSquare x = latinSquare' x /\ latinSquare' (transpose x)

quasigroupCompletion :: [[Int]] -> [Int]
quasigroupCompletion p = let
                           n = length p
                           x' = take (n*n) (domain 1 n)
                           x = chunksOf n x'
                         in
                           solveFD [FirstFail,Bisect,Down] x' $
                           -- handle the hints
                           foldl1 (/\) [xx =# fd v | (v,xx) <- zip (concat p) x', v > 0] /\
                           latinSquare x 


printSol' :: Show a => [a] -> IO ()
printSol' sol = do
                (mapM_ print sol)
                putStrLn ""
                
printSol :: Prelude.Show b => ([a] -> [b]) -> [a] -> Prelude.IO ()
printSol f p = printSol' $ chunksOf (length p) $ f p

printSols :: Prelude.Show b => ([a] -> [b]) -> [a] -> Prelude.IO ()
printSols f p = mapM_ printSol' $ map (chunksOf (length p)) $ allValues $ f p


-- One solution
main1 :: Prelude.IO ()
main1 = printSols quasigroupCompletion problem1

-- One solution
main2 :: Prelude.IO ()
main2 = printSols quasigroupCompletion problem2

-- Two solutions
main3 :: Prelude.IO ()
main3 = printSols quasigroupCompletion problem3

-- Twelve solutions
main4 :: Prelude.IO ()
main4 = printSols quasigroupCompletion problem4

--
-- Problem 5..7 has too many solutions so only the first solution is shows.
-- However, I'm not sure how to present these solution in a nice way, i.e.
-- how to combine mapM_ print with Just (...)
--
main5 :: Maybe [[Int]]
main5 = fmap (chunksOf (length problem5)) $ oneValue $ quasigroupCompletion problem5

main6 :: Maybe [[Int]]
main6 = fmap (chunksOf (length problem6)) $ oneValue $ quasigroupCompletion problem6

main7 :: Maybe [[Int]]
main7 = fmap (chunksOf (length problem7)) $ oneValue $ quasigroupCompletion problem7

-- Show the number of solutions of problem7: (it's 40944)
main7b :: Int
main7b = length $ allValues $ quasigroupCompletion problem7

-- No solution
main8 :: [[Int]]
main8 = allValues $ quasigroupCompletion problem8

-- No solution
main9 :: [[Int]]
main9 = allValues $ quasigroupCompletion problem9


main :: IO ()
main = do
         putStrLn "problem 1:"
         main1
         putStrLn "\nproblem 2:"
         main2
         putStrLn "\nproblem 3:"
         main3
         putStrLn "\nproblem 4:"
         main4
         putStrLn "\nproblem 5:"
         print $ main5
         putStrLn "\nproblem 6:"
         print $ main6
         putStrLn "\nproblem 7:"
         print $ main7
         putStrLn "\nproblem 7b:"
         print $ main7b
         putStrLn "\nproblem 8:"
         print $ main8
         putStrLn "\nproblem 9:"
         print $ main9
         

--
-- Example from Ruben Martins and Inès Lynce
-- Breaking Local Symmetries in Quasigroup Completion Problems, page 3
-- The solution is unique:
-- 1 3 2 5 4
-- 2 5 4 1 3
-- 4 1 3 2 5
-- 5 4 1 3 2
-- 3 2 5 4 1
--
-- Note: this is an array of arrays
--
problem1 :: [[Int]]
problem1 = [[1, 0, 0, 0, 4],  
            [0, 5, 0, 0, 0],
            [4, 0, 0, 2, 0],
            [0, 4, 0, 0, 0],
            [0, 0, 5, 0, 1]]

--
-- Example from Gomes & Shmoys, page 3.
-- Solution:
-- 4 1 2 3
-- 2 3 4 1
-- 1 4 3 2
-- 3 2 1 4
--
problem2 :: [[Int]]
problem2 =[[0, 1, 2, 3],
           [2, 0, 4, 1], 
           [1, 4, 0, 2],
           [3, 0, 1, 0]]

-- Example from Gomes & Shmoys, page 7
-- Two solutions.
--
problem3 :: [[Int]]
problem3 = [[0, 1, 0, 0],
            [0, 0, 2, 0],
            [0, 3, 0, 0],
            [0, 0, 0, 4]]


--
-- Example from Global Constraint Catalogue
-- http://www.emn.fr/x-info/sdemasse/gccat/sec2.7.108.html
--
-- 12 solutions.
--
problem4 :: [[Int]]
problem4 = [[1, 0, 0, 0],
            [0, 0, 0, 3],
            [3, 0, 0, 0],
            [0, 0, 0, 1]]


--
-- Problem from http://www.cs.cornell.edu/gomes/QUASIdemo.html
-- (n = 10]
-- Pattern #1. 
-- There are _many_ solutions to this problem.
--
problem5 :: [[Int]]
problem5 = [[0,0,0,1,0,0,0,0,0,0],
            [0,0,1,0,0,0,0,0,0,0],
            [0,1,0,0,0,2,0,0,0,0],
            [1,0,0,0,2,0,0,0,0,0],
            [0,0,0,2,1,0,0,0,0,0],
            [0,0,2,0,0,1,0,0,0,0],
            [0,0,0,0,0,0,1,0,0,0],
            [0,0,0,0,0,0,0,1,0,2],
            [0,0,0,0,0,0,0,0,2,0],
            [0,0,0,0,0,0,0,2,0,0]]


--
-- Problem from http://www.cs.cornell.edu/gomes/QUASIdemo.html
-- (n = 10]
-- Pattern #2. 
-- There are _many_solutions to this problem.
--
problem6 :: [[Int]]
problem6 = [[0,0,1,2,3,4,0,0,0,0],
            [0,1,2,3,0,0,4,0,0,0],
            [1,2,3,0,0,0,0,4,0,0],
            [2,3,0,0,0,0,0,0,4,0],
            [3,0,0,0,0,0,0,0,0,4],
            [5,6,0,0,0,0,0,0,0,0],
            [0,5,6,0,0,0,0,0,0,0],
            [0,0,5,6,0,0,0,0,0,0],
            [0,0,0,5,6,0,0,0,0,0],
            [0,0,0,0,5,6,0,0,0,0]]


--
-- Problem from http://www.cs.cornell.edu/gomes/QUASIdemo.html
-- (n = 10]
-- Pattern #3. 
-- Coding:
--    dark red   = 1
--    light blue = 2 
--    dark blue  = 3 
--    light red  = 4
--    brown      = 5
--    green      = 6
--    pink       = 7
--    grey       = 8
--    black      = 9
--    yellow     = 10    
-- There are 40944 solutions for this pattern.
--
problem7 :: [[Int]]
problem7 = [[0, 0, 1, 5, 2, 6, 7, 8, 0, 0],
            [0, 1, 5, 2, 0, 0, 6, 7, 8, 0],
            [1, 5, 2, 0, 0, 0, 0, 6, 7, 8],
            [5, 2, 0, 0, 0, 0, 0, 0, 6, 7],
            [2, 0, 0, 0, 0, 0, 0, 0, 0, 6],
            [4,10, 0, 0, 0, 0, 0, 0, 3, 9],
            [0, 4,10, 0, 0, 0, 0, 3, 9, 0],
            [0, 0, 4,10, 0, 0, 3, 9, 0, 0],
            [0, 0, 0, 4,10, 3, 9, 0, 0, 0], 
            [0, 0, 0, 0, 4,9, 0, 0, 0, 0]]


--
-- Problem from http://www.cs.cornell.edu/gomes/QUASIdemo.html
-- (n = 10]
-- Pattern #4. 
--  dark red   = 1
--  light blue = 2
--  dark blue  = 3
--  light red  = 4
-- Note: There are no solutions to this problem.
--
problem8 :: [[Int]]
problem8 = [[1,0,0,0,0,0,0,0,0,0],
            [2,1,0,0,0,0,0,0,0,4],
            [3,2,1,0,0,0,0,0,4,0],
            [0,3,2,1,0,0,0,4,0,0],
            [0,0,3,2,1,0,4,0,0,0],
            [0,0,0,3,2,1,0,0,0,0],
            [0,0,0,0,3,2,1,0,0,0],
            [0,0,0,4,0,3,2,1,0,0],
            [0,0,4,0,0,0,3,2,1,0],
            [0,4,0,0,0,0,0,3,2,1]]


--
-- Problem from http://www.cs.cornell.edu/gomes/QUASIdemo.html
-- (n = 10]
-- Pattern #5
-- Note: There are no solutions to this problem.
--
problem9 :: [[Int]]
problem9 = [[0,0,0,0,0,0,0,0,0,1],
            [0,0,0,0,0,0,0,0,1,0],
            [0,0,0,0,0,0,0,1,0,0],
            [0,0,0,0,0,0,2,0,0,0],
            [0,0,0,0,0,1,0,0,0,0],
            [0,0,0,0,1,0,0,0,0,0],
            [0,0,0,1,0,0,0,0,0,0],
            [0,0,1,0,0,0,0,0,0,0],
            [0,1,0,0,0,0,0,0,0,0],
            [1,0,0,0,0,0,0,0,0,0]]




