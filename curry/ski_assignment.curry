{- 
  
  Ski assignment puzzle in Curry

  From  
  Jeffrey Lee Hellrung, Jr.: PIC 60, Fall 2008, Final Review, December 12, 2008
  http://www.math.ucla.edu/~jhellrun/course_files/Fall%25202008/PIC%252060%2520-%2520Data%2520Structures%2520and%2520Algorithms/final_review.pdf
  """
  5. Ski Optimization! Your job at Snapple is pleasant but in the winter you've 
  decided to become a ski bum. You've hooked up with the Mount Baldy Ski Resort. 
  They'll let you ski all winter for free in exchange for helping their ski rental 
  shop with an algorithm to assign skis to skiers. Ideally, each skier should 
  obtain a pair of skis whose height matches his or her own height exactly. 
  Unfortunately, this is generally not possible. We define the disparity between 
  a skier and his or her skis to be the absolute value of the difference between 
  the height of the skier and the pair of skis. Our objective is to find an 
  assignment of skis to skiers that minimizes the sum of the disparities. 
  ...
  Illustrate your algorithm by explicitly filling out the A[i, j] table for the 
  following sample data:
    * Ski heights: 1, 2, 5, 7, 13, 21.
    * Skier heights: 3, 4, 7, 11, 18.
  """

  The solution of ski_assignment (from the non CP version ski_assignment) is 
    (7,[1,2,3,4,5],[1,1,0,2,3])

  i.e. the skiers should select these skis:
  Skier Skier height  Ski height   Height diff
  0      3             2            1
  1      4             5            1   
  2      7             7            0
  3     11            13            2
  4     18            21            3
  ------------------------------------------
  Total diff                        7                          

  The CLP.FD version is very slow since I have to use very slow decompositions 
  of the abs and element constraints


  Cf my Picat model http://hakank.org/picat/ski_assignment.pi

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/

-}

import Data.List
import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import HakankUtilsCLPFD (abs2,elementVal)
import CLP.FD


--
-- non deterministic version (anyOf permutations ...)
--
ski_assignment :: Num a => (a, [Int], [a])
ski_assignment = let
                    skiHeights   = [1, 2, 5, 7, 13, 21]
                    skierHeights = [3, 4, 7, 11, 18]
                    numSkis = length skiHeights
                    numSkiers = length skierHeights
                    -- We have 5 skiers and they can select any of the 6 skis
                    p = anyOf $ permutations [0..numSkis-1] -- non det select a permutation
                    x = take numSkiers p
                    diffs = [ abs ( (skiHeights !! (x !! i)) - (skierHeights !! i))
                                        | i <- [0..numSkiers-1]  ]                    
                    z = Data.List.sum diffs
                 in
                    (z,x,diffs) 

--
-- A list comprehension version (non CP)
--
ski_assignment2 :: (Num a, Ord a) => (a, [Int])
ski_assignment2 = let
                    skiHeights   = [1, 2, 5, 7, 13, 21]
                    skierHeights = [3, 4, 7, 11, 18]
                    numSkis = length skiHeights
                    numSkiers = length skierHeights
                    -- We have 5 skiers and they can select any of the 6 skis
                    zx = head $ sort [ (Data.List.sum [ abs ( (skiHeights !! (x !! i)) - (skierHeights !! i))
                                                       | i <- [0..numSkiers-1]  ],
                                       x) 
                                     | p <- permutations [0..numSkis-1], let x = take numSkiers p]
                 in
                    zx

--
-- CP model
-- 
-- CLP.FD does not support abs of decision variables and there is no element constraint either.
-- Here are instead used the (very) slow decompositions HakanUtilsCLPFD.abs2 and HakanUtilsCLPFD.elementVal
--
ski_assignment_clp :: [Int]
ski_assignment_clp = let
                    skiHeights   = [1, 2, 5, 7, 13, 21]
                    skierHeights = [3, 4, 7, 11, 18]
                    numSkis = length skiHeights
                    numSkiers = length skierHeights

                    xs = take numSkiers (domain 1 numSkis)
                    selectedSkiHeight = take numSkiers (domain 1 21) -- The height of the selected ski
                    diffs = take numSkiers (domain 0 10)
                    z = head (domain 0 10) -- total difference (to minimize)
                 in
                    solveFD [FirstFail,Bisect] (z:xs) $
                    allDifferent xs /\
                    z =# Data.List.sum diffs /\
                    
                    -- get the selected ski height for each skier
                    --    selectedSkiHeight[i] = skiHeights[xs[i]]
                    foldl (/\) [elementVal (xs !! i) skiHeights (selectedSkiHeight !! i) | i <- [0..numSkiers-1]] /\
                    
                    -- the difference in length between skier height and ski height
                    --   diffs[i] = abs(selectedSkiHeight[i] - skierHeights[i])
                    foldl1 (/\) [ abs2 (selectedSkiHeight !! i) (skierHeights !! i) (diffs !! i) | i <- [0..numSkiers-1]]


{-

  (7,[1,2,3,4,5],[1,1,0,2,3])
  Execution time: 150 msec. / elapsed: 293 msec.

-}
main :: (Num a, Ord a) => (a, [Int], [a])
main = head . sort $ allValues ski_assignment

{-

  (7,[1,2,3,4,5])
  Execution time: 181 msec. / elapsed: 193 msec.

-}
main2 :: (Int,[Int])
main2 = ski_assignment2

{-
  It works but is very slow:

  (7,[1,2,3,4,5])
  Execution time: 257634 msec. / elapsed: 257654 msec.

-}
main3 :: ([Int],[Int])
main3 = splitAt 1 . head . sort $ allValues ski_assignment_clp