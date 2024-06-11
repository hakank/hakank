{- 
  
  Euler #26 in Haskell

  """
  A unit fraction contains 1 in the numerator. The decimal representation of the 
  unit fractions with denominators 2 to 10 are given:

      1/2	= 	0.5
      1/3	= 	0.(3)
      1/4	= 	0.25
      1/5	= 	0.2
      1/6	= 	0.1(6)
      1/7	= 	0.(142857)
      1/8	= 	0.125
      1/9	= 	0.(1)
      1/10	= 	0.1

  Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be 
  seen that 1/7 has a 6-digit recurring cycle.

  Find the value of d < 1000 for which 1/d contains the longest recurring cycle in 
  its decimal fraction part.
  """ 

  Ah, we have to use (fromIntegral n) when using large integers!

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
import HakankUtils

--
-- Get the first position of value val in the list (x:xs)
--
-- getPos :: Integer -> Integer -> [Integer] -> Integer
getPos val pos (x:xs) 
    | x == val = pos
    | null xs = pos                 
    | otherwise = getPos val (pos+1) xs

--
-- The rep length
--
getRepLen n = 1+(getPos 1 0 [(10^i `mod` n) | i <- [1..n]])

getRepLen3_ n i
          | (10^i) `mod` n == 1 = i
          | i == n = 0                                
          | otherwise         = getRepLen3_ n (i+1)
getRepLen3 n = getRepLen3_ n 1

euler26a = snd $ last $ sort $ [(getRepLen (fromIntegral n),n) | n <- [2..999], isPrime n]

-- Variant using maximum instead
euler26b = snd $ maximum $ [(getRepLen (fromIntegral n),n) | n <- [2..999], isPrime n]

euler26d = snd $ maximum [(getRepLen3 (fromIntegral n),n) | n <- [2..999], isPrime n]

-- One-liner
euler26e = snd $ maximum [ (head [i | i <- [1..n], 1==(10^i) `mod` (fromIntegral n)], n) | n <- [11..999],isPrime n] 
           
main = do
         print euler26a -- (0.00 secs, 35,272 bytes)

         print euler26b -- (0.00 secs, 35,272 bytes)

         print euler26d -- (0.00 secs, 35,272 bytes)

         print euler26e -- (0.00 secs, 35,272 bytes)