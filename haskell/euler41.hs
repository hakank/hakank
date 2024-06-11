{- 
  
  Euler #41 in Haskell

  """
  We shall say that an n-digit number is pandigital if it makes use of all 
  the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital 
  and is also prime.

  What is the largest n-digit pandigital prime that exists?
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
import HakankUtils

euler41a' n = [ t | p <- permutations [1..n], t <- [(digitsToNum p)], isPrime t ]

euler41a = maximum $ concat $ map euler41a' [1..9]

           
-- Simplification:
-- n=9 is not possible since 1+2+3+4+5+6+7+8+9=45 is divisible by 3
-- n=8 is not possible since 1+2+3+4+5+6+7+8=36 is divisible by 3
euler41b = maximum $ concat $ map euler41a' [1..7]

-- A slightly different approach
euler41c' :: Int -> Int
euler41c' n = maximum $ [ t | p <- permutations [1..n], t <- [(digitsToNum p)], isPrime t ]

euler41c :: Int
euler41c = head $ map euler41c' [7,6..1]


main = do
         print euler41a -- (4.25 secs, 5,712,920,864 bytes)

         print euler41b -- (0.30 secs, 298,250,672 bytes)

         print euler41c -- (0.33 secs, 290,607,928 bytes)

