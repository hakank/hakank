{- 
  
  Euler #30 in Curry

  Problem 30  
  """
  Surprisingly there are only three numbers that can be written 
  as the sum of fourth powers of their digits:

     1634 = 1^(4) + 6^(4) + 3^(4) + 4^(4)
     8208 = 8^(4) + 2^(4) + 0^(4) + 8^(4)
     9474 = 9^(4) + 4^(4) + 7^(4) + 4^(4)

  As 1 = 1^(4) is not a sum it is not included.

  The sum of these numbers is 1634 + 8208 + 9474 = 19316.

  Find the sum of all the numbers that can be written as the sum of 
  fifth powers of their digits.
  """
 

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils
-- import CLP.FD


euler30a :: Int
euler30a = sum [n | n <- [10..6*(9^5)], n == sum ( map (^5) ( numToDigits n))]

euler30b' :: Int -> Int -> Int
euler30b' n c 
        | n > 6*(9^5) = c
        | otherwise   = euler30b' (n+1) (if t then (c+n) else c)
                where t =  n == sum ( map (^5) ( numToDigits n)) 

euler30b :: Int
euler30b = euler30b' 10 0

main :: IO ()
main = do
         -- PAKCS: Execution time: 153647 msec. / elapsed: 178477 msec.
         -- KICS2: KiCS2 compilation time: 1.80s / elapsed: 0:02.15 GHC compilation time: 1.29s / elapsed: 0:01.74 Execution time: 2.26s / elapsed: 0:02.28
         -- Curry2Go: Compilation time: 1.90s / elapsed: 0:01.57 Execution time: 1344.32s / elapsed: 6:57.53
         -- print euler30a


         -- PAKCS: -
         -- KICS2: KiCS2 compilation time: 1.89s / elapsed: 0:02.25 GHC compilation time: 1.31s / elapsed: 0:01.77 Execution time: 2.73s / elapsed: 0:02.79
         -- Curry2Go: -
         print euler30b
