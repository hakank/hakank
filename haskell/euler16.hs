{- 
  
  Euler #16 in Haskell

  Problem 16
  """
  2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
  
  What is the sum of the digits of the number 2^1000?
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
import Data.Char
-- import HakankUtils

euler16a = sum $ map digitToInt $ show $ 2^1000

main = do
         print euler16a -- (0.00 secs, 144,880 bytes)

