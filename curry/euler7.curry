{- 
  

  Euler #7 in Curry

  Problem 7
  """
  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see 
  that the 6^(th) prime is 13.

  What is the 10001^(st) prime number?
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils

euler7a :: Int
euler7a = last $ take 10001 $ iterate nextPrime 2

-- A little cheating
euler7b :: Int
euler7b = last $ take 10001 $ primesN 200000

-- simple loop version
euler7c' :: (Eq a, Num a) => Int -> a -> Int
euler7c' n a = if a == 10001 then
                  n
               else
                  euler7c' (nextPrime n) (a+1)

euler7c :: Int
euler7c = euler7c' 2 1

-- Using the quite slower isPrime2
euler7d :: Int
euler7d = last $ take 10001 $ primesN2 200000

main :: IO ()
main = do
         -- PAKCS: Execution time: 14304 msec. / elapsed: 17418 msec. 
         -- KICS2: 1.65s+1.41s+0.25s
         -- Curry2Go: Compilation time: 1.91s / elapsed: 0:01.50 Execution time: 72.37s / elapsed: 0:28.16
         -- print euler7a
         
         -- PAKCS: Execution time: 14658 msec. / elapsed: 16320 msec. 
         -- KICS2: 1.71s+1.43s+0.24s
         -- Curry2Go: Compilation time: 2.11s / elapsed: 0:01.64 Execution time: 53.68s / elapsed: 0:27.11
         -- print euler7b
         
         -- PAKCS: Execution time: 14188 msec. / elapsed: 17331 msec 
         -- KICS2: 1.76s+1.42s+0.24s
         -- Curry2Go: Compilation time: 2.16s / elapsed: 0:01.67 Execution time: 68.23s / elapsed: 0:27.82
         print euler7c
         
         -- PAKCS: Execution time: 21836 msec. / elapsed: 24329 msec. 
         -- KICS2: 1.69s+1.34s+0.41s
         -- Curry2Go: Compilation time: 1.71s / elapsed: 0:01.42 Execution time: 82.13s / elapsed: 0:42.56
         -- print euler7d