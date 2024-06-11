{- 
  
  Euler #5 in Curry.

  Problem 5
  """
  2520 is the smallest number that can be divided by each of the numbers 
  from 1 to 10 without any remainder.

  What is the smallest number that is evenly divisible by all of the numbers 
  from 1 to 20?
  """


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils


euler5a :: Int
euler5a = foldr1 lcm [2..20] -- foldr, a little faster

euler5b :: Int
euler5b = foldl1 lcm [2..20] -- foldl

euler5c :: Int
euler5c = last $ scanl1 lcm [2..20]

euler5d :: Int
euler5d = last . take 19 $ scanl1 lcm [2..]

main :: IO ()
main = do
         -- PAKCS: Execution time: 1 msec. / elapsed: 1 msec. 
         -- KICS2: KiCS2 compilation time: 1.66s / elapsed: 0:02.06 GHC compilation time: 1.25s / elapsed: 0:01.70 Execution time: 0.00s / elapsed: 0:00.01
         -- Curry2Go: Compilation time: 1.90s / elapsed: 0:01.45 Execution time: 0.08s / elapsed: 0:00.03
         print euler5a
         
         -- PAKCS: Execution time: 28 msec. / elapsed: 28 msec. 
         -- KICS2: KiCS2 compilation time: 1.80s / elapsed: 0:02.12 GHC compilation time: 1.28s / elapsed: 0:01.73 Execution time: 0.00s / elapsed: 0:00.00
         -- Curry2Go: Compilation time: 2.17s / elapsed: 0:01.65 Execution time: 0.07s / elapsed: 0:00.03
         -- print euler5b

         -- PAKCS: Execution time: 2 msec. / elapsed: 2 msec. 
         -- KICS2: KiCS2 compilation time: 1.65s / elapsed: 0:01.99 GHC compilation time: 1.28s / elapsed: 0:01.71 Execution time: 0.00s / elapsed: 0:00.01
         -- Curry2Go: Compilation time: 2.19s / elapsed: 0:01.66 Execution time: 0.09s / elapsed: 0:00.03
         -- print euler5c
         
         -- PAKCS: Execution time: 1 msec. / elapsed: 1 msec. 
         -- KICS2: KiCS2 compilation time: 1.65s / elapsed: 0:02.03 GHC compilation time: 1.24s / elapsed: 0:01.64 Execution time: 0.00s / elapsed: 0:00.01
         -- Curry2Go: Compilation time: 2.15s / elapsed: 0:01.68 Execution time: 0.08s / elapsed: 0:00.02
         -- print euler5d

