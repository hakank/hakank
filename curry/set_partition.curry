{- 
  
  Set partition problem in Curry CLP.FD

  Problem formulation from
  http://www.koalog.com/resources/samples/PartitionProblem.java.html
  """
   This is a partition problem.
   Given the set S = {1, 2, ..., n}, 
   it consists in finding two sets A and B such that:

    *  A U B = S
    *  |A| = |B|
    * sum(A) = sum(B)
    * sum_squares(A) = sum_squares(B)
  """
  
  This is a port of my Picat model http://hakank.org/picat/set_partition.pi

  NOPE: Missing instance for Prelude.Num CLP.FD.FDConstr
  Again, Curry CLP.FD have problem with this construct:
       ((fd i)*(a !! i)) =# (fd s) 


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import CLP.FD


set_partition =
  let
    n = 12
    numSets = 2
    n2 = n `div` numSets
    a = take n (domain 0 (numSets-1))
    counts = take numSets (domain n2 n2)
    sums = take numSets (domain 0 (n^2))
    sumSquared = take numSets (domain 0 (n^3))
  in
    solveFD [] a $
    {-
       NOPE! 
       Missing instance for Prelude.Num CLP.FD.FDConstr
    in application
    Data.List.sum [((fd i) * (a !! i)) =# (fd s) | i <- [0 .. n - 1]]
     
    Term: [((fd i) * (a !! i)) =# (fd s) | i <- [0 .. n - 1]]
    | 
 46 |                   (sums !! s) =# Data.List.sum [ ((fd i)*(a !! i)) =# (fd s) | i <- [0..n-1]] /\
    |                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    -}
    foldl1 (/\) [
                  count (fd s) a Equ (counts !! s) /\
                  (sums !! s) =# Data.List.sum [ ((fd i)*(a !! i)) =# (fd s) | i <- [0..n-1]] /\
                  (sumSquared !! s) =# Data.List.sum [ ((fd (i*i))*(a !! i)) =# (fd s) | i <- [0..n-1]]
                | s <- [0..numSets-1] ] -- /\
             
    foldl1 (/\) [  (sums !! s) =# (sums !! (s+1)) /\
                   (sumSquared !! s) =# (sumSquared !! (s+1)) 
                | s <- [0..numSets-1] ] /\
                  
    -- symmetry breaking
    head a =# 0
                          

main = set_partition