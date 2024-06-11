{- 
  
  Euler #20 in Haskell

  Problem 20
  """
  n! means n (n 1) ... 3 2 1

  Find the sum of the digits in the number 100!")
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Data.Char
import HakankUtils

euler20a = sum $ map digitToInt $ show $ fact 100

euler20b = sum $ numToDigits $ fact 100

euler20c = sum $ numToDigits $ foldr1 (*) [1..100]

main = do
         print euler20a -- (0.00 secs, 125,512 bytes)
         print euler20b -- (0.01 secs, 125,744 bytes)
         print euler20c -- (0.00 secs, 122,824 bytes)

