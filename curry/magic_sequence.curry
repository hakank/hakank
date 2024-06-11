{-
  Magic sequence problem in Curry.

  http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob019/spec.html
  """
  A magic sequence of length n is a sequence of integers x0 . . xn-1 between 
  0 and n-1, such that for all i in 0 to n-1, the number i occurs exactly xi 
  times in the sequence. For instance, 6,2,1,0,0,0,1,0,0,0 is a magic sequence 
  since 0 occurs 6 times in it, 1 occurs twice, ...
  """

  This works but is very slow...

  Note: There are two solutions for n=4
   [1,2,1,0]
   [2,0,2,0]

  and no solution for n=6

  KiCS2 is faster, but it takes 37s for magicSequence 7

  Program created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Curry page: http://www.hakank.org/curry/

-}

import Data.List
import Control.AllValues
import HakankUtils

-- The magic: ensure that all xs!!i is the sum of i's in xs
-- This is non-det
-- magic (-1) xs = True
-- magic i xs = (if xs!!i == count i xs then True else False)  && magic (i-1) xs

-- This is det
magic i xs
   | i==(-1)   = True
   | otherwise = (if xs!!i == count i xs then True else False)  && magic (i-1) xs

magicSequence n = let xs free in (xs =:= intVars n [0..n-1] & magic (n-1) xs) &> xs 


main = magicSequence 4

main2 = magicSequence 5

-- No solution
main3 = magicSequence 6


main4 = magicSequence 7