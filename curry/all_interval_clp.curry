{- 
  
  All interval problem in Curry CLP.FD

  CSPLib problem number 7
  http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob007/index.html
  """
  Given the twelve standard pitch-classes (c, c , d, ...), represented by 
  numbers 0,1,...,11, find a series in which each pitch-class occurs exactly 
  once and in which the musical intervals between neighbouring notes cover 
  the full set of intervals from the minor second (1 semitone) to the major 
  seventh (11 semitones). That is, for each of the intervals, there is a 
  pair of neigbhouring pitch-classes in the series, between which this 
  interval appears. The problem of finding such a series can be easily 
  formulated as an instance of a more general arithmetic problem on Z_n, 
  the set of integer residues modulo n. Given n in N, find a vector 
  s = (s_1, ..., s_n), such that (i) s is a permutation of 
  Z_n = {0,1,...,n-1}; and (ii) the interval vector 
  v = (|s_2-s_1|, |s_3-s_2|, ... |s_n-s_{n-1}|) is a permutation of 
  Z_n-{0} = {1,2,...,n-1}. A vector v satisfying these conditions is 
  called an all-interval series of size n; the problem of finding such 
  a series is the all-interval series problem of size n. We may also be 
  interested in finding all possible series of a given size. 
  """

  Note: 
  - Here we using a non deterministic version of abs (HakankUtilsCLPFD.abs2) 
    since there's no abs on decision variables in CLP.FD.

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (chunksOf)
import HakankUtilsCLPFD (abs2)
import CLP.FD


-- check []      []   = True
-- ERROR: abs undefined for FD constraints
-- check (x:y:xs) (d:ds) = abs(x-y) =# d /\ check (y:xs) ds

-- Let's try our abs2 instead.
check _        []     = true
check (x:y:xs) (d:ds) = (abs2 x y d) /\ check (y:xs) ds

allInterval :: Int -> [Int]
allInterval n = let
                  n1 = n-1
                  xs = take n (domain 1 n)
                  diffs = take n1 (domain 1 n1)
                in
                  solveFD [FirstFail,Bisect] (concat [xs,diffs]) $
                  allDifferent xs /\
                  allDifferent diffs /\
                  check xs diffs /\
                  -- foldl1 (/\) [ abs2 a b d | (a,b,d) <- zip3 xs (tails xs) diffs ] -- does not work
                  
                  -- symmetry breaking
                  (xs !! 0) <# (xs !! (n1-1)) /\
                  (diffs !! 0) <# (diffs !! 1)
                  
getSol :: Int -> ([Int],[Int])
getSol n = splitAt n $ allInterval n

{-
  ([4,3,1,5,2],[1,2,4,3])
  ([3,4,2,5,1],[1,2,3,4])
  ([2,5,1,3,4],[3,4,2,1])

-}
main :: ([Int], [Int])
main = getSol 5

{-
  There are 104 solutions for allInterval 10

  ([7,6,2,8,5,3,10,1,9,4],[1,4,6,3,2,7,9,8,5])
  ([7,6,2,10,1,8,5,3,9,4],[1,4,8,9,7,3,2,6,5])
  ([7,6,1,10,2,9,3,5,8,4],[1,5,9,8,7,6,2,3,4])
  ([7,6,3,5,10,1,9,2,8,4],[1,3,2,5,9,8,7,6,4])
  ([7,3,8,6,5,2,9,1,10,4],[4,5,2,1,3,7,8,9,6])
  ([7,1,10,2,9,6,5,3,8,4],[6,9,8,7,3,1,2,5,4])
  ([5,1,10,2,9,3,8,6,7,4],[4,9,8,7,6,5,2,1,3])
  .. 
  ([6,8,3,4,7,1,10,2,9,5],[2,5,1,3,6,9,8,7,4])
  ([5,9,3,4,7,2,10,1,8,6],[4,6,1,3,5,8,9,7,2])
  ([6,9,3,4,8,1,10,2,7,5],[3,6,1,4,7,9,8,5,2])
  ([4,9,3,5,8,1,10,2,6,7],[5,6,2,3,7,9,8,4,1])
  ([3,5,8,4,9,2,10,1,7,6],[2,3,4,5,7,8,9,6,1])
  ([3,5,8,4,10,1,9,2,7,6],[2,3,4,6,9,8,7,5,1])
  ([3,5,10,1,9,2,8,4,7,6],[2,5,9,8,7,6,4,3,1])
  ([5,6,8,3,7,4,10,1,9,2],[1,2,5,4,3,6,9,8,7])
  Execution time: 3329 msec. / elapsed: 3342 msec.

-}
main2 :: ([Int], [Int])
main2 = getSol 10

{-
  Let's check that it's 104 solutions. 

  104
  Execution time: 3315 msec. / elapsed: 3315 msec.

-}
main2b :: Int
main2b = length . allValues $ getSol 10

{- 
  Get one value from allInterval 12

  Just ([8,7,5,1,12,2,11,3,10,4,9,6],[1,2,4,11,10,9,8,7,6,5,3])
  Execution time: 12789 msec. / elapsed: 12793 msec.

-}
main3 :: Maybe ([Int], [Int])
main3 = oneValue $ getSol 12