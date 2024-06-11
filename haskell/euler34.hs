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
isFactSum n = n == sum [fact i | i <- numToDigits n]

euler34a = sum [n | n <- [10..100000], isFactSum n]


--
-- Caching fact n (as Int)
--
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

isFactSum_c n = n == sum [fact_c i | i <- numToDigits n]

--
-- Using a cached version of fact: Faster
--
euler34b = sum [n | n <- [10..100000], isFactSum_c n]


--
-- Caching fact n (as char)
--
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

isFactSum_c2 n = n == sum [fact_c2 i | i <- show n]

euler34c = sum [n | n <- [10..100000], isFactSum_c2 n]

--
-- Another caching
--
fact_c3 n = [1,1,2,6,24,120,720,5040,40320,362880] !! n

isFactSum_c3 n = n == sum [fact_c3 i | i <- numToDigits n]

euler34d = sum [n | n <- [10..100000], isFactSum_c2 n]


main = do
         -- print euler34a -- (0.36 secs, 628,017,080 bytes)

         -- print euler34b -- (0.59 secs, 430,683,280 bytes)

         print euler34b -- (0.23 secs, 171,443,520 bytes)
 
         -- print euler34d -- (0.25 secs, 171,443,520 bytes)
