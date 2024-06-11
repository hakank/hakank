{- 
  
  Euler #30 in Haskell

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
  Also see my Haskell page: http://hakank.org/curry/
  
  
-}

import Data.List
import HakankUtils


euler30a :: Int
euler30a = sum [n | n <- [10..6*(9^5)], n == sum ( map (^5) ( numToDigits n))]

euler30b' n c 
        | n > 6*(9^5) = c
        | otherwise   = euler30b' (n+1) (if t then (c+n) else c)
                where t =  n == sum ( map (^5) ( numToDigits n)) 

euler30b :: Int
euler30b = euler30b' 10 0

main :: IO ()
main = do
         print euler30a -- (1.15 secs, 2,080,461,680 bytes)
         -- print euler30b -- (1.47 secs, 2,409,237,288 bytes)
