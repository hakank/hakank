{- 
  
   Euler #37 in Curry

  """
  The number 3797 has an interesting property. Being prime itself, it is possible to 
  continuously remove digits from left to right, and remain prime at each stage: 
  3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

  Find the sum of the only eleven primes that are both truncatable from left to right 
  and right to left.

  NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import HakankUtils

--
-- Is n a truncable prime?
--
truncablePrime :: Int -> Bool
truncablePrime n = isPrime n && and [(isPrime $ n `mod` (10^i)) && (isPrime $ n `div` (10^i)) | i <- [1..(nlen n)-1]]

--                   
-- Check for a new number until we get 11 truncable primes
-- 
euler37a' :: Int -> [Int] -> [Int]
euler37a' n xs
   | length xs == 11 = xs
   | otherwise       = if truncablePrime n then euler37a' (n+2) (n:xs) else euler37a' (n+2) xs


euler37a :: Int
euler37a = sum $ euler37a' 11 []

main :: IO ()
main = do
          print euler37a -- (8.85 secs, 8,508,146,248 bytes)
