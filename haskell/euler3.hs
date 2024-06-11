{-
  Euler #3 in Haskell

  Problem 3
  """
  The prime factors of 13195 are 5, 7, 13 and 29.
  What is the largest prime factor of the number 600851475143 ?
  """

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Haskell page: http://www.hakank.org/haskell/

-}

import Data.List
import HakankUtils

euler3a = last (primeFactors 600851475143)

-- > main
-- 6857
main = do
         print euler3a -- (0.02 secs, 2,558,288 bytes)

-- > main1
-- [71,839,1471,6857]
main1 = primeFactors 600851475143 --