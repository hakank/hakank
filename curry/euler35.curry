{- 
  
  Euler #35 in Curry

  Problem 35
  """
  The number, 197, is called a circular prime because all rotations 
  of the digits: 197, 971, and 719, are themselves prime.

  There are thirteen such primes below 100: 
  2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

  How many circular primes are there below one million?
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (isPrime,primesN,rotate,numToDigits,digitsToNum)
-- import CLP.FD

-- Check is all rotations of number n are primes
allRotatedPrimes :: Int -> Bool
allRotatedPrimes n = and [isPrime $ digitsToNum $ rotate i xs | i <- [0..len-1] ]
                  where
                    xs = numToDigits n
                    len = length xs

euler35a :: Int
euler35a = length $ [n | n <- [2..999999], isPrime n, allRotatedPrimes n]

--
-- Just testing odd numbers.
--
euler35b :: Int
euler35b = length $ [2] ++ [n | n <- [3,5..999999], isPrime n, allRotatedPrimes n]


euler35c :: Int
euler35c = length $ [n | n <- primesN 999999, allRotatedPrimes n]

main :: IO ()
main = do
         -- PAKCS: Execution time: 698109 msec. / elapsed: 787984 msec
         -- KICS2: KiCS2 compilation time: 1.82s / elapsed: 0:02.13 GHC compilation time: 1.32s / elapsed: 0:01.82 Execution time: 11.13s / elapsed: 0:11.17
         -- Curry2Go: > 115min
         -- print euler35a

         -- PAKCS: Execution time: 685240 msec. / elapsed: 775844 msec.
         -- KICS2: KiCS2 compilation time: 1.67s / elapsed: 0:02.03 GHC compilation time: 1.42s / elapsed: 0:01.88 Execution time: 11.14s / elapsed: 0:11.15
         -- Curry2Go: Compilation time: 2.08s / elapsed: 0:01.54 Execution time: 3718.17s / elapsed: 37:29.66         
         -- print euler35b


         -- PAKCS: Execution time: 691875 msec. / elapsed: 783722 msec.
         -- KICS2: KiCS2 compilation time: 1.88s / elapsed: 0:02.25 GHC compilation time: 1.39s / elapsed: 0:01.85 Execution time: 11.07s / elapsed: 0:11.09
         -- Curry2Go: >25min
         print euler35c
