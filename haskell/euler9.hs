{- 
  
  Euler #9 in Haskell

  Problem 9
  """
  A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
  a^2 + b^2 = c^2

  For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product abc.
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
-}

import Data.List

-- 4.941s
euler9a = head [a*b*c | c <-[1..500], b <- [1..c],a <- [1..b], a+b+c == 1000, a^2+b^2==c^2 ]

main = do
         print euler9a -- (4.49 secs, 3,630,707,584 bytes)
