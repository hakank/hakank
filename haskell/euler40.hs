{- 
  
  Euler #40 in Haskell

  """
  An irrational decimal fraction is created by concatenating the positive integers:
   
  0.123456789101112131415161718192021...
   
  It can be seen that the 12th digit of the fractional part is 1.

  If dn represents the nth digit of the fractional part, find the 
  value of the following expression.
  
  d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
import Data.Char
-- import HakankUtils

euler40a =  product [digitToInt $ ds !! ((10^i)-1)  | i <- [0..5]]
            where
            ds = concat [show n | n <- [1..210000]]

-- euler40b: This is a Curry specific non-det version. And very slow.
                 
-- Recursive version
euler40c' ds i aux
      | i == 5    = aux * (d i)
      | otherwise = euler40c' ds (i+1) ((d i) * aux)
      where
      d i = digitToInt $ ds !! ((10^i)-1)

euler40c =  euler40c' (concat [show n | n <- [1..210000]]) 0 1


main = do
         print euler40a -- (0.03 secs, 14,473,632 bytes)

         print euler40c -- (0.03 secs, 14,475,152 bytes)
         
