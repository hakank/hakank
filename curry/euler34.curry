{- 
  
  Euler #34 in Curry

  Problem 34
  """
  145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
  
  Find the sum of all numbers which are equal to the sum of the 
  factorial of their digits.

  Note: as 1! = 1 and 2! = 2 are not sums they are not included.
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils
-- import CLP.FD

--
-- First version (no caching of fact)
--
isFactSum :: Int -> Bool
isFactSum n = n == sum [fact i | i <- numToDigits n]

euler34a :: Int
euler34a = sum [n | n <- [10..100000], isFactSum n]


--
-- Caching fact n (as Int)
--
fact_c :: Int -> Int
fact_c 0 = 1
fact_c 1 = 1
fact_c 2 = 2
fact_c 3 = 6
fact_c 4 = 24
fact_c 5 = 120
fact_c 6 = 720
fact_c 7 = 5040
fact_c 8 = 40320
fact_c 9 = 362880

isFactSum_c :: Int -> Bool
isFactSum_c n = n == sum [fact_c i | i <- numToDigits n]

--
-- Using a cached version of fact: Faster
--
euler34b :: Int
euler34b = sum [n | n <- [10..100000], isFactSum_c n]


--
-- Caching fact n (as char)
--
fact_c2 :: Char -> Int
fact_c2 '0' = 1
fact_c2 '1' = 1
fact_c2 '2' = 2
fact_c2 '3' = 6
fact_c2 '4' = 24
fact_c2 '5' = 120
fact_c2 '6' = 720
fact_c2 '7' = 5040
fact_c2 '8' = 40320
fact_c2 '9' = 362880

isFactSum_c2 :: Int -> Bool
isFactSum_c2 n = n == sum [fact_c2 i | i <- show n]

euler34c :: Int
euler34c = sum [n | n <- [10..100000], isFactSum_c2 n]

--
-- Another caching
--
fact_c3 :: Int -> Int
fact_c3 n = [1,1,2,6,24,120,720,5040,40320,362880] !! n

isFactSum_c3 :: Int -> Bool
isFactSum_c3 n = n == sum [fact_c3 i | i <- numToDigits n]

euler34d :: Int
euler34d = sum [n | n <- [10..100000], isFactSum_c2 n]

main :: IO ()
main = do
         -- PAKCS: Execution time: 16009 msec. / elapsed: 18380 msec.
         -- KICS2: KiCS2 compilation time: 1.79s / elapsed: 0:02.07 GHC compilation time: 1.34s / elapsed: 0:01.75 Execution time: 0.33s / elapsed: 0:00.34
         -- Curry2Go: Compilation time: 3.52s / elapsed: 0:06.19 Execution time: 82.33s / elapsed: 0:35.42
         -- print euler34a

         -- PAKCS: Execution time: 5667 msec. / elapsed: 6737 msec.
         -- KICS2: KiCS2 compilation time: 1.72s / elapsed: 0:02.13 GHC compilation time: 1.22s / elapsed: 0:01.65 Execution time: 0.16s / elapsed: 0:00.16
         -- Curry2Go: Compilation time: 2.28s / elapsed: 0:01.73 Execution time: 26.08s / elapsed: 0:11.42
         -- print euler34b

         -- PAKCS: Execution time: 2981 msec. / elapsed: 3710 msec.
         -- KICS2: GHC compilation time: 1.82s / elapsed: 0:02.35 Execution time: 0.10s / elapsed: 0:00.11
         -- Curry2Go: Compilation time: 2.39s / elapsed: 0:01.67 Execution time: 14.57s / elapsed: 0:06.07
         print euler34b


         -- PAKCS: Execution time: 2812 msec. / elapsed: 3503 msec.
         -- KICS2: KiCS2 compilation time: 1.89s / elapsed: 0:02.20 GHC compilation time: 1.35s / elapsed: 0:01.75 Execution time: 0.09s / elapsed: 0:00.11 
         -- Curry2Go: Compilation time: 2.40s / elapsed: 0:01.65 Execution time: 14.44s / elapsed: 0:06.01
         print euler34d
