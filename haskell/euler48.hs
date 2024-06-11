{- 
  
  Euler #48 in Haskell

  """
  The series, 1^(1) + 2^(2) + 3^(3) + ... + 10^(10) = 10405071317.
  
  Find the last ten digits of the series, 
  1^(1) + 2^(2) + 3^(3) + ... + 1000^(1000).
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
import HakankUtils

euler48a = drop (len-10) xs
           where
           xs = numToDigits $ sum [i^i | i <- [1..1000]]
           len = length xs

main = do
         print euler48a -- (0.03 secs, 5,464,712 bytes)

