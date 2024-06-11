{- 
  
  Euler #35 in Haskell

  Problem 35
  """
  The number, 197, is called a circular prime because all rotations 
  of the digits: 197, 971, and 719, are themselves prime.

  There are thirteen such primes below 100: 
  2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

  How many circular primes are there below one million?
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
import HakankUtils


allRotatedPrimes :: Int -> Bool
allRotatedPrimes n = and [isPrime $ digitsToNum $ rotate i xs | i <- [0..len-1] ]
                  where
                    xs = numToDigits n
                    len = length xs

euler35a :: Int
euler35a = length $ [n | n <- [2..999999], isPrime n, allRotatedPrimes n]

-- Just testing odd numbers. Not much faster than euler35a.
euler35b :: Int
euler35b = length $ [2] ++ [n | n <- [3,5..999999], isPrime n, allRotatedPrimes n]

euler35c = length $ [n | n <- primesN 999999, allRotatedPrimes n]
           
main :: IO ()
main = do
         -- print euler35a -- (25.72 secs, 25,190,106,040 bytes)
         print euler35b -- (25.06 secs, 24,917,475,384 bytes)
         -- print euler35c -- (25.13 secs, 24,923,130,624 bytes)