{- 
  
  Euler #41 in Curry CLP.FD

  """
  We shall say that an n-digit number is pandigital if it makes use of all 
  the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital 
  and is also prime.

  What is the largest n-digit pandigital prime that exists?
  """
  
  This is a CLP.FD version.
  Faster than any of the PAKCS non CLP.FD and deterministic versions,
  but slower than the fastest KICS2 version.

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (isPrime)
import HakankUtilsCLPFD
import CLP.FD

check' :: Int -> [[Int]]
check' n = let
                x = take n (domain 1 n)
                nn = head (domain (10^(n-1)) ((10^n)-1) )
              in
                solveFDAll [Bisect] [nn] $
                allDifferent x /\
                nn =# toNum x 10

check :: Int -> Int
check n = maximum $ [p | p <- concat $ check' n,isPrime p]

euler41_clp :: Int
euler41_clp = head $ map check [7,6..1]

main :: Int
main = do
         -- PAKCS: Execution time: 6258 msec. / elapsed: 7115 msec.
         -- KICS2:  Does not support CLP.FD
         -- Curry2Go: Does not support CLP.FD
         euler41_clp

