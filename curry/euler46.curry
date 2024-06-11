{- 
  
  Euler #46 in Curry

  """  
  It was proposed by Christian Goldbach that every odd composite number can be 
  written as the sum of a prime and twice a square.

  9 = 7 + 2×1^2
  15 = 7 + 2×2^2
  21 = 3 + 2×3^2
  25 = 7 + 2×3^2
  27 = 19 + 2×2^2
  33 = 31 + 2×1^2

  It turns out that the conjecture was false.

  What is the smallest odd composite that cannot be written as the 
  sum of a prime and twice a square?
  """


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils
-- import CLP.FD

check_a :: Int -> Bool
check_a n = if not $ null [(n,p,s) | p <- ps, s <- squares, n == p + s*2] then True else False
         where
           n2 = n `div` 2
           ps = take n2 primes
           squares = [i*i | i <- [1..n2]]

euler46a :: Int
euler46a = head $ filter (\n -> check_a n == False) $ cs
           where
           cs = filter (\n -> not $ isPrime n) [3,5..]

--
-- Reversing order of squares and primes: slower.
-- Interestingly, in Haskell this is a magnitude faster than euler46a.
--
check_b :: Int -> Bool
check_b n = if not $ null [(n,p,s) | s <- squares, p <- ps, n == p + s*2] then True else False
         where
           n2 = n `div` 2
           ps = take n2 primes
           squares = [i*i | i <- [1..n2]]

euler46b :: Int
euler46b = head $ filter (\n -> check_b n == False) $ cs
           where
           cs = filter (\n -> not $ isPrime n) [3,5..]

--
-- Same approach: Testing a smaller range
--
check_c :: Int -> Bool
check_c n = if not $ null ( take 1 [(n,p,s) | p <- ps, s <- squares, n == p + s*2]) then True else False
         where
           nd = n `div` 2
           ns = ceiling (sqrt (fromIntegral (n `div` 2)))
           ps = take nd primes
           squares = [i*i | i <- [1..ns]]

euler46c :: Int
euler46c = head $ filter (\n -> check_c n == False) $ cs
           where
           cs = filter (\n -> not $ isPrime n) [3,5..]

--
-- Another approach: Much faster
-- (This is a port of my imperative Picat program euler46/0 in euler46.pi)
--
check_d :: Int -> Int -> Bool
check_d i j
  | j > round (sqrt ( (fromIntegral i) / 2 )) = True
  | isPrime (abs(i - (2*j*j)))                = False
  | otherwise                                 = check_d i (j+1)

euler46d' :: Int -> Int
euler46d' i
  | (not $ isPrime i) && check_d i 1  = i
  | otherwise                         = euler46d' (i+2)

euler46d :: Int
euler46d = euler46d' 3
  


main :: IO ()
main = do
         -- PAKCS: Execution time: 1046598 msec. / elapsed: 1149810 msec.
         -- KICS2: KiCS2 compilation time: 1.82s / elapsed: 0:02.14 GHC compilation time: 1.31s / elapsed: 0:01.75 Execution time: 17.22s / elapsed: 0:17.28
         -- Curry2Go: Compilation time: 3.53s / elapsed: 0:02.34 Execution time: 3012.27s / elapsed: 28:40.09
         -- print euler46a


         -- PAKCS: > 30min
         -- KICS2: KiCS2 compilation time: 1.71s / elapsed: 0:02.11 GHC compilation time: 1.39s / elapsed: 0:01.90 Execution time: 340.67s / elapsed: 5:40.83
         -- Curry2Go: >30min
         -- print euler46b


         -- PAKCS: Execution time: 178685 msec. / elapsed: 206198 msec.
         -- KICS2: KiCS2 compilation time: 1.66s / elapsed: 0:02.00 GHC compilation time: 1.41s / elapsed: 0:01.85 Execution time: 2.83s / elapsed: 0:02.83
         -- Curry2Go: Compilation time: 2.33s / elapsed: 0:01.55 Execution time: 674.29s / elapsed: 6:24.89
         -- print euler46c


         -- PAKCS: Execution time: 1157 msec. / elapsed: 1315 msec
         -- KICS2: KiCS2 compilation time: 1.83s / elapsed: 0:02.18 GHC compilation time: 1.51s / elapsed: 0:01.88 Execution time: 0.03s / elapsed: 0:00.04
         -- Curry2Go: Compilation time: 2.41s / elapsed: 0:01.59 Execution time: 4.14s / elapsed: 0:02.47
         print euler46d