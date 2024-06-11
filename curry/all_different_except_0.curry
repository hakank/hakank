{- 
  
  allDifferentExcept0 in Curry CLP.FD

  Simple test of the allDifferentExcept0 constraint.
  Note: It uses non determinism and it's not fast...

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import HakankUtilsCLPFD (allDifferentExcept0,increasingCLP)
import CLP.FD


{-
  x must be increasing and has at least one 0:

  [0,0,1,2]
  [0,0,1,3]
  [0,0,1,4]
  [0,0,2,3]
  [0,0,2,4]
  [0,0,3,4]
  [0,0,0,1]
  [0,0,0,2]
  [0,0,0,3]
  [0,0,0,4]
  [0,0,0,0]
  Execution time: 433 msec. / elapsed: 434 msec.

-}
main :: [Int]
main = let
         n = 4
         x = take n (domain 0 n)
       in
         solveFD [FirstFailConstrained,Bisect] x $
         allDifferentExcept0 x /\
         increasingCLP x /\
         count 0 x Gt (fd 1)

{- 
  It's not fast. Calculating the number of solutions for n=5

  1546
  Execution time: 65657 msec. / elapsed: 65919 msec.

-}
test2 :: [[Int]]
test2 = let
         n = 5
         x = take n (domain 0 n)
       in
         solveFDAll [FirstFail,Bisect] x $
         allDifferentExcept0 x

main2 :: Int
main2 = length test2