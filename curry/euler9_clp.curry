{- 
  
  Euler #9 in Curry CLP.FD

  Problem 9
  """
  A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
  a^2 + b^2 = c^2

  For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product abc.
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import CLP.FD


{-
  CLP to the rescue.

  This is a port of my Picat model in euler9.pi 

  triplet(A,B,C) = Prod =>
    LD = [A,B,C],
    LD :: 1..500,
    A + B + C #= 1000,
    A #=< B, % symmetry breaking
    B #=< C, 
    A**2 + B**2 - C**2 #= 0,
    Prod #= A * B *C,
    solve([degree,split], LD).

-}
-- Only supported by PAKCS
euler9b' :: [Int]
euler9b' = let
            [a,b,c] = take 3 (domain 1 500)
          in solveFD [Bisect] [a,b,c] $
            a+b+c =# 1000 /\
            a <=# b /\
            b <=# c /\
            a^2 + b^2 =# c^2

euler9b :: Int
euler9b = product euler9b'

main :: IO ()
main = do
         -- PAKCS: Execution time: 6 msec. / elapsed: 7 msec. 
         -- KICS2: CLP.FD is not supported
         -- Curry2Go: CLP.FD is not supported
         print euler9b