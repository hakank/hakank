{- 
  
  Assignment problems in Curry

  This is an alternative model (compared to assignment.curry) where 
  allDifferent on a list of decision variables is used instead of 
  using a matrix with 0/1 decision variables.

  However, this version is quite slower, probably since it uses the (very) 
  slow decomposition of elementVal. For instance 6 which has 8 people this
  is simply too slow (all possible 8! permutations is generated via the allDifferent
  constraint).

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
import HakankUtilsCLPFD (elementVal,allDifferent2)
import CLP.FD
import AssignmentProblems -- 6 assignments problems

assignment :: [[Int]] -> [[Int]]
assignment p = let
                 rows = length p        -- number of people
                 cols = length $ head p
                 -- Max cost per assignee (for the domain of costs)
                 maxCosts = map maximum p
                 -- for the domain of totalCost
                 sumCost = Data.List.sum $ maxCosts
                 
                 -- decision variables
                 xs = take rows (domain 0 (cols-1)) -- which task is assigned to person i
                 costs = take rows (domain 0 (maximum maxCosts)) -- the cost per assigned task
                 totalCost = head (domain 0 sumCost)
               in
                 solveFDAll [Bisect] (totalCost:xs) $
                 allDifferent xs /\

                 -- the costs per assigned task
                 foldl1 (/\) [elementVal (xs !! i) row (costs !! i) | i <- [0..rows-1], let row = map fd (p !! i)] /\
                  
                 -- total cost
                 totalCost =# Data.List.sum costs

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
getSol :: ([Char], [[Int]]) -> (Int, [Int])
getSol (opt,p) = (optVal,sol)
                 where
                   sol1 = sort $ assignment p
                   (optVal:sol) = if opt == "minimize" then head sol1 else head $ reverse sol1


main1 :: (Int, [Int])
main1 = getSol cost1

main2 :: (Int, [Int])
main2 = getSol cost2

main3 :: (Int, [Int])
main3 = getSol cost3

main4 :: (Int, [Int])
main4 = getSol cost4

main5 :: (Int, [Int])
main5 = getSol cost5

main6 :: (Int, [Int])
main6 = getSol cost6

{-
  Skipping problem 6 since it's too slow.

  (11,[1,0,2,4])
  (207,[2,3,0,1])
  (181,[2,5,4,6])
  (30,[0,1,3,2])
  (8,[1,0,3,4,2])
  Execution time: 1459 msec. / elapsed: 1462 msec.

-}
main :: IO ()
main = do 
          print main1
          print main2
          print main3
          print main4
          print main5
          -- print main6 -- too slow !

