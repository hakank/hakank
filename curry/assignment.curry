{- 
  
  Assignment problems in Curry

  This is a port of my Picat model http://hakank.org/picat/assignment.pi

  Again, I'm missing a proper a way to optimize a value in the CLP.FD model.
  Here we sort all solutions and pick the optimal value.

  The problems are in AssignmentProblems.curry

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
-}

import Data.List
import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (chunksOf,matrixElement,findPos)
import CLP.FD
import AssignmentProblems -- 6 assignments problems

assignment :: [[Int]] -> [[Int]]
assignment p = let
                 rows = length p
                 cols = length $ head p
                 rc = rows*cols
                 -- Max cost per assignee (for the domain of totalCost)
                 sumCost = Data.List.sum $ map maximum p 
                 -- decision variables
                 x' = take rc (domain 0 1)
                 xs = chunksOf cols x'
                 totalCost = head (domain 0 sumCost)
               in
                 solveFDAll [FirstFail,Bisect] (totalCost:x') $
                 
                 -- exacly one assignment per row, all rows must be assigned
                 foldl1 (/\) [ Data.List.sum row =# (fd 1)  | row <- xs] /\ 

                 -- zero or one assignments per column, i.e. all tasks might not be assigned.
                 foldl1 (/\) [ Data.List.sum col <=# (fd 1)  | col <- (transpose xs)] /\

                 -- total cost
                 totalCost =# Data.List.sum [(matrixElement xs i j) * fd (matrixElement p i j)
                                             | i <- [0..rows-1], j <- [0..cols-1]]

--
-- Show a nicer output:
--    (11,[1,0,2,4]) (for problem 1)
-- means that 11 is the optimal cost, and the assignments are
--  assignee  assigned task
--  0         1
--  1         0
--  2         2
--  3         4
--
getSol :: (Enum a, Num a) => ([Char], [[Int]]) -> (Int, [a])
getSol (opt,p) = (optVal,ps)
                 where
                   cols = length $ head p
                   sol1 = sort $ assignment p
                   (optVal:sol) = if opt == "minimize" then head sol1 else head $ reverse sol1
                   cs = chunksOf cols sol -- the assignment matrix
                   ps = [task  | c <- cs, let task = findPos 1 c ] -- the assignments per assignee


main1 :: (Enum a, Num a) => (Int, [a])
main1 = getSol cost1

main2 :: (Enum a, Num a) => (Int, [a])
main2 = getSol cost2

main3 :: (Enum a, Num a) => (Int, [a])
main3 = getSol cost3

main4 :: (Enum a, Num a) => (Int, [a])
main4 = getSol cost4

main5 :: (Enum a, Num a) => (Int, [a])
main5 = getSol cost5

main6 :: (Enum a, Num a) => (Int, [a])
main6 = getSol cost6

{-

  (11,[1,0,2,4])
  (207,[2,3,0,1])
  (181,[2,5,4,6])
  (30,[0,1,3,2])
  (8,[1,0,3,4,2])
  (76,[0,7,6,4,1,5,3,2])
  Execution time: 2539 msec. / elapsed: 2692 msec.

-}
main :: IO ()
main = do 
          print main1
          print main2
          print main3
          print main4
          print main5
          print main6

              
