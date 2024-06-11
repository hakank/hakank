{- 
  
  Euler #47 in Curry

  """  
  The first two consecutive numbers to have two distinct prime factors are:

  14 = 2 x 7
  15 = 3 x 5

  The first three consecutive numbers to have three distinct 
  prime factors are:

  644 = 2^2 x 7 x 23
  645 = 3 x 5 x 43
  646 = 2 x 17 x 19.

  Find the first four consecutive integers to have four distinct primes 
  factors. What is the first of these numbers?
  """ 


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import HakankUtils

numPrimeFactors :: Int -> Int
numPrimeFactors n = length $ nub $ primeFactors n

euler47a' xs i
  | c1 == 4 && c2 == 4 && c3 == 4 && c4 == 4 = i
  | otherwise                                = euler47a' (tail xs) (i+1)
  where
  [(i1,c1),(i2,c2),(i3,c3),(i4,c4)] = take 4 xs  
  
euler47a = euler47a' (map (\n -> (n, numPrimeFactors n)) [1..]) 1

--
-- Simpler, and slightly faster than euler47a
--
euler47b' xs i
  | c1 == 4 && c2 == 4 && c3 == 4 && c4 == 4 = i
  | otherwise                                = euler47b' (tail xs) (i+1)
  where
  [c1,c2,c3,c4] = take 4 xs  
  
euler47b = euler47b' (map numPrimeFactors [1..]) 1

--
-- Even simpler and slightly faster than euler47b
--
euler47c' xs i
  | [4,4,4,4] == take 4 xs = i
  | otherwise              = euler47c' (tail xs) (i+1)
  where

euler47c = euler47b' (map numPrimeFactors [1..]) 1


main = do
         -- print euler47a -- (2.10 secs, 1,580,027,768 bytes)

         -- print euler47b -- (2.08 secs, 1,567,159,592 bytes) 

         print euler47c -- (2.02 secs, 1,567,159,424 bytes)
