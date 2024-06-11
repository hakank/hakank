{- 
  
  Euler #9 in Curry

  Problem 9
  """
  A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
  a^2 + b^2 = c^2

  For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product abc.
  """

  For a fast CLP.FD version, see euler9_clp.curry

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils


-- Brute force: Very slow (about 2minutes)
-- The Haskell version takes 5.21s
euler9a :: Int
euler9a = head [a*b*c | c<-[1..500],b<-[1..c],a<-[1..b], a+b+c == 1000, a^2+b^2==c^2 ]

main :: IO ()
main = do
         -- PAKCS: Execution time: 128001 msec. / elapsed: 142185 msec. 
         -- KICS2: 1.05s+1.34s+1.72s !!!
         -- Curry2Go: Compilation time: 1.99s / elapsed: 0:01.52 Execution time: 403.15s / elapsed: 4:11.85 !!!
         print euler9a
         
         -- PAKCS: Execution time: 6 msec. / elapsed: 7 msec.   See euler9_clp.curry
         -- KICS2: CLP.FD is not supported
         -- Curry2Go: CLP.FD is not supported
         -- print euler9b