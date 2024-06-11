{- 
  
  Euler #31 in Curry

  Problem 31
  """
  In England the currency is made up of pound, £, and pence, p, and 
  there are eight coins in general circulation:

     1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

  It is possible to make £2 in the following way:

     1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

  How many different ways can £2 be made using any number of coins?
  """

  See euler31_clp.curry (PAKCS only: about 2.1s)

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import HakankUtils

--
-- Dynamic programming.
-- Port of my Picat function coins2/3 (euler31.pi)
--
coins_a :: (Num a, Ord a, Num b) => [a] -> a -> Int -> b
coins_a coins money m = if m == (len-1) then 1 else coins_a' m len coins money 0
                      where
                      len = length coins

coins_a' :: (Num a, Ord a, Num b) => Int -> Int -> [a] -> a -> b -> b
coins_a' i len coins money s0
   | i >= len = s0
   | otherwise = s
   where
     ci = coins !! i
     s1 = if money - ci == 0 then 1 else 0
     s2 = if money - ci > 0 then s1 + coins_a coins (money-ci) i else s1
     s = if i < len then s0 + coins_a' (i+1) len coins money s2 else s0 + s2

euler31a :: Int
euler31a = coins_a [200,100,50,20,10,5,2,1] 200 0

         
main :: IO ()
main = do
         print euler31a -- (0.35 secs, 306,435,280 bytes)
