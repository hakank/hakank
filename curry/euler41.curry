{- 
  
  Euler #41 in Curry

  """
  We shall say that an n-digit number is pandigital if it makes use of all 
  the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital 
  and is also prime.

  What is the largest n-digit pandigital prime that exists?
  """

  Also see the CLP.FD version euler41_clp.curry (for PAKCS) which is 
  faster than any of the deterministic PAKCS versions (~7.2s).

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils
-- import CLP.FD

--
-- Brute force: Checking permutations of lengths 1..9
--
euler41a' :: Int -> [Int]
euler41a' n = [ t | p <- permutations [1..n], t <- [(digitsToNum p)], isPrime t ]

euler41a :: Int
euler41a = maximum $ concat $ map euler41a' [1..9]

--
-- Simplification:
-- n=9 is not possible since 1+2+3+4+5+6+7+8+9=45 is divisible by 3
-- n=8 is not possible since 1+2+3+4+5+6+7+8=36 is divisible by 3
--
euler41b :: Int
euler41b = maximum $ concat $ map euler41a' [1..7]


-- A slightly different approach than euler41b
euler41c' :: Int -> Int
euler41c' n = maximum $ [ t | p <- permutations [1..n], t <- [(digitsToNum p)], isPrime t ]

euler41c :: Int
euler41c = head $ map euler41c' [7,6..1]


main :: IO ()
main = do
         -- PAKCS: Execution time: 224402 msec. / elapsed: 261983 msec.
         -- KICS2: GHC compilation time: 1.29s / elapsed: 0:01.75 Execution time: 3.68s / elapsed: 0:03.69
         -- Curry2Go: Compilation time: 1.98s / elapsed: 0:01.49 Execution time: 1048.59s / elapsed: 9:08.39
         -- print euler41a

         -- PAKCS: Execution time: 8904 msec. / elapsed: 10173 msec
         -- KICS2: KiCS2 compilation time: 1.64s / elapsed: 0:02.05 GHC compilation time: 1.30s / elapsed: 0:01.67 Execution time: 0.16s / elapsed: 0:00.16
         -- Curry2Go: Compilation time: 1.98s / elapsed: 0:01.47 Execution time: 32.66s / elapsed: 0:18.10
         print euler41b


         -- PAKCS: Execution time: 7986 msec. / elapsed: 9132 msec.
         -- KICS2: KiCS2 compilation time: 1.71s / elapsed: 0:02.08 GHC compilation time: 1.29s / elapsed: 0:01.70 Execution time: 0.15s / elapsed: 0:00.16
         -- Curry2Go: Compilation time: 1.74s / elapsed: 0:01.41 Execution time: 31.89s / elapsed: 0:17.97
         -- print euler41c
