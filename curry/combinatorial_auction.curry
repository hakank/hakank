{- 
  
  Combinatorial auction in Curry

  http://en.wikipedia.org/wiki/Combinatorial_auction
  """
  A combinatorial auction is an auction in which bidders can place 
  bids on combinations of items, or "packages," rather than 
  just individual items. Simple combinatorial auctions have been 
  used for many years in estate auctions, where a common procedure 
  is to auction the individual items and then at the end to accept 
  bids for packages of items.
  """

  This simple example is from the lecture slides
  Constraint Satisfaction Problems, Constraint Optimization
  by Bernhard Nebel and Stefan Wölfl
  http://www.informatik.uni-freiburg.de/~ki/teaching/ws0910/csp/csp10-handout4.pdf
  """
  In combinatorial auctions, bidders can give bids for set of items.
  The auctioneer [then] has to generate an optimial selection, e.g.
  one that maximizes revenue.
  
  Definition
  The combinatorial auction problem  is specified as follows:
    Given: A set of items Q = {q1,...,qn} and a set of bids
           B = {b1,...,bm} such that each bid is bi = (Qi, ri),
           where Qi (= Q and ri is a strictly positive real number.
    Task: Find a subset of bids B'(= B such that any two bids in B'
          do not share an item maximizing Sum(Qi,ri) (= Biri.

  ...

  Example Auction

  Consider the following auction:
    b1 = {1,2,3,4}, r1 = 8
    b2 = {2,3,6},   r2 = 6
    b3 = {1,4,5},   r3 = 5
    b4 = {2,8},     r4 = 2
    b5 = {5,6},     r5 = 2

  What is the optimal assignment?
  """ 

  Here I experiment with two ways of getting the optimal value.
  - combinatorialAuction: generate all solutions + sort + pick the first
  - combinatorialAuction2: using solveFDOne Maximize non-det totalOpt 
    (since Maximize requires an integer, not FDVar)

  In this model, the first approach seems to be faster.

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Control.AllValues
-- import HakankUtils
import CLP.FD
import Debug.Trace


combinatorialAuction p = let
                           (packages,bids) = p
                           numBids = length packages
                           numItems = length $ nub (concat packages)
                           sumBids = Data.List.sum(bids)
                           
                           -- decision variables
                           xs = take numBids (domain 0 1) -- xs[i] = 1 if we should take bid[i] else 0
                           total = head (domain 0 sumBids)
                         in 
                           solveFDAll [FirstFail,Bisect] ([total]++xs) $
                           Data.List.sum xs ># 0 /\ -- we must pick at least one bid/package
                           -- ensure that each items is selected at most once
                           foldl1 (/\) [ Data.List.sum([ xs !! i | i <- [0..numBids-1], elem j (packages !! i) ] ) <=# 1
                                       | j <- [0..numItems-1]]

                           /\ Data.List.sum([(xs!!i)* (fd (bids!!i)) | i <- [0..numBids-1]]) =# total


--
-- Using Maximize totalOpt
-- (totalOpt is non-det).
--
combinatorialAuction2 p = let
                           (packages,bids) = p
                           numBids = length packages
                           numItems = length $ nub (concat packages)
                           sumBids = Data.List.sum(bids)
                           
                           -- decision variables
                           xs = take numBids (domain 0 1) -- xs[i] = 1 if we should take bid[i] else 0
                           total = head (domain 0 sumBids)
                           totalOpt = anyOf $ reverse [0..sumBids]
                         in 
                           solveFDOne [FirstFail,Bisect,Maximize totalOpt] ([total]++xs) $
                           fd totalOpt =# total /\
                           Data.List.sum xs ># 0 /\ -- we must pick at least one bid/package
                           -- ensure that each items is selected at most once
                           foldl1 (/\) [ Data.List.sum([ xs !! i | i <- [0..numBids-1], elem j (packages !! i) ] ) <=# 1
                                       | j <- [0..numItems-1]]

                           /\ Data.List.sum([(xs!!i)* (fd (bids!!i)) | i <- [0..numBids-1]]) =# total


getOptSolution p = putStrLn ("opt: " ++ (show opt) ++ "\n" ++ "assignments: " ++ (show assignments))
                   where
                     (opt:assignments) = last . sort $ combinatorialAuction p

getOptSolution2 p = putStrLn ("opt: " ++ (show opt) ++ "\n" ++ "assignments: " ++ (show assignments))
                   where
                     (opt:assignments) = combinatorialAuction2 p

{-
opt: 11
assignments: [0,1,1,0,0]
Execution time: 43 msec. / elapsed: 48 msec.
-}
main1 =  getOptSolution problem1

{-
opt: 11
assignments: [0,1,1,0,0]
Execution time: 46 msec. / elapsed: 46 msec.

-}
main2 =  getOptSolution problem2

{-
opt: 40
assignments: [1,1,1,1,0,0,0]
Execution time: 52 msec. / elapsed: 61 msec.
-}
main3 =  getOptSolution problem3

{-
opt: 300
assignments: [0,0,1]
Execution time: 18 msec. / elapsed: 18 msec.

-}
main4 =  getOptSolution problem4

--
-- Using Maximize (non-det)
--
{-
  opt: 11
  assignments: [0,1,1,0,0]
  Execution time: 38 msec. / elapsed: 38 msec.

-}
main1b = getOptSolution2 problem1

{-
opt: 11
assignments: [0,1,1,0,0]
Execution time: 48 msec. / elapsed: 49 msec.

-}
main2b = getOptSolution2 problem1

{-
opt: 40
assignments: [1,1,1,1,0,0,0]
Execution time: 40 msec. / elapsed: 40 msec.

-}
main3b = getOptSolution2 problem3

{-
opt: 300
assignments: [0,0,1]
Execution time: 99 msec. / elapsed: 99 msec.

-}
main4b = getOptSolution2 problem4


--
-- The example cited above
--
problem1 :: ([[Int]],[Int])
problem1 = ([[0,1,2,3],  -- packages
             [1,2,5],
             [0,3,4],
             [1,6],
             [4,5]],
             [8,6,5,2,2] -- bids
             )


----
---- From Numberjack Tutorial, page 24 (slide 51/175)
----
problem2 :: ([[Int]],[Int])
problem2 = ([[0,1],
             [0,2],
             [1,3],
             [1,2,3],
             [0]],
            [8,6,5,2,2])


--
-- From "A Faster Core Constraint Generation Algorithm for Combinatorial Auctions"
-- Benedikt Bunz, Sven Seuken, Benjamin Lubin
-- page 4
--
problem3 :: ([[Int]],[Int])
problem3 = ([[0],
             [1],
             [2],
             [3],
             [0,1,2,3,4],
             [0,1,4],
             [2,3,4]],

            [10,10,10,10,12,8,8])


--
-- From "Combinatorial Auctions:  Complexity and Algorithms"
-- Martin Bichler
-- page 4 (table 1)
--
-- Line  Bids                       B1    B2   B3   B4
--    1  1000t grain in Berlin      1     0    1    1
--    2  800t grain in Munich       0     1    1    1
--    3  800t grain in Vienna       1     1    1    0
--    4  Bid price (in thousands) 150   125   300 125
--
problem4 :: ([[Int]],[Int])
problem4 = ([[0,2,3],
             [1,2,3],
             [0,1,2]],
             [150,125,300,125])
