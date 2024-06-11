{- 
  
  Divisible by 9 through 1 puzzle in Curry CLP.FD

  From http://msdn.microsoft.com/en-us/vcsharp/ee957404.aspx
  " Solving Combinatory Problems with LINQ"
  """
  Find a number consisting of 9 digits in which each of the digits 
  from 1 to 9 appears only once. This number must also satisfy these 
  divisibility requirements:
  
   1. The number should be divisible by 9.
   2. If the rightmost digit is removed, the remaining number should 
      be divisible by 8.
   3. If the rightmost digit of the new number is removed, the remaining 
      number should be divisible by 7.
   4. And so on, until there's only one digit (which will necessarily 
      be divisible by 1).
  """
  
  Also, see
  "Intel® Parallel Studio: Great for Serial Code Too (Episode 1)"
  http://software.intel.com/en-us/blogs/2009/12/07/intel-parallel-studio-great-for-serial-code-too-episode-1/

  This is a port of my Picat model http://hakank.org/picat/divisible_by_9_through_1.pi

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import HakankUtilsCLPFD (mod2,toNum)
import CLP.FD


divisible_by_9_through_1 base = let
                                  m = base^(base-1)-1 -- largest value
                                  n = base - 1
                                  xs = take n (domain 1 n)
                                  t = take n (domain 1 m) -- the slices of xs[0..i-1]
                                in
                                  solveFDAll [] (xs) $
                                  allDifferent xs /\
                                  foldl1 (/\) [(t !! (i-1)) =# toNum xi (fd base)  /\
                                               mod2 (t !! (baseI-1)) (fd i) 0 m
                                                | i <- [1..n],
                                                let baseI = base - i,
                                                let xi = [ xs !! j | j <- [0..baseI-1] ]
                                               ]
{-
  [[3,8,1,6,5,4,7,2,9]]
  Execution time: 22 msec. / elapsed: 26 msec.

-}
main = divisible_by_9_through_1 10

{-
  Testing bases 2..16 
  (2,[[1]])
  (3,[])
  (4,[[1,2,3],[3,2,1]])
  (5,[])
  (6,[[1,4,3,2,5],[5,4,3,2,1]])
  (7,[])
  (8,[[3,2,5,4,1,6,7],[5,2,3,4,7,6,1],[5,6,7,4,3,2,1]])
  (9,[])
  (10,[[3,8,1,6,5,4,7,2,9]])
  (11,[])
  (12,[])
  (13,[])
  (14,[[9,12,3,10,5,4,7,6,11,8,1,2,13]])
  (15,[])
  (16,[])
  Execution time: 1341 msec. / elapsed: 1349 msec.

-}
main2 = mapM_ print $ map (\n -> (n,divisible_by_9_through_1 n)) [2..16]