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
import Control.AllValues
import Control.SetFunctions
import HakankUtils
-- import CLP.FD


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

--
-- Same approach as in the CLP.FD model in euler31_clp.curry
-- But it's much slower (I stopped after running > 100 minutes w/o solution).
-- The CLP.FD model takes about 2s in PAKCS.
--
euler31b' :: (Data a, Enum a, Num a, Ord a) => (a, a, a, a, a, a, a, a)
euler31b' = 
             [a,b,c,d,e,f,g,h] =:= makeVars 8 [0..200] &>
             b <= 100 &> c <= 40 &> d <= 20 &> e <= 10 &> f <= 4 &> g <= 2 &> h <= 1 &>
             (1*a) + (2*b) + (5*c) + (10*d) + (20*e) + (50*f) + (100*g) + (200*h) =:= 200
             &> (a,b,c,d,e,f,g,h) 
             where a,b,c,d,e,f,g,h free

euler31b :: Int
euler31b = length $ allValues $ euler31b'

euler31c :: IO Int
euler31c = fmap (length) $ values2list $ set0 euler31b'

main :: IO ()
main = do
         -- PAKCS: Execution time: 8032 msec. / elapsed: 9107 msec.
         -- KICS2: KiCS2 compilation time: 2.28s / elapsed: 0:02.57 GHC compilation time: 2.49s / elapsed: 0:02.88 Execution time: 0.15s / elapsed: 0:00.17
         -- Curry2Go: Compilation time: 2.68s / elapsed: 0:07.59 Execution time: 47.12s / elapsed: 0:15.86
         print euler31a


         -- PAKCS:  >100min
         -- KICS2:  >100min
         -- Curry2Go: runtime: out of memory  Compilation time: 2.11s / elapsed: 0:01.64 Execution time: 753.36s / elapsed: 3:06.44
         -- print euler31b


         -- PAKCS:  >125min
         -- KICS2:  >125min
         -- Curry2Go: runtime: out of memory  Compilation time: 2.01s / elapsed: 0:01.54 Execution time: 759.34s / elapsed: 2:57.87
         -- euler31c

