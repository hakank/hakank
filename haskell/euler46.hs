{- 
  
  Euler #46 in Curry

  """  
  It was proposed by Christian Goldbach that every odd composite number can be 
  written as the sum of a prime and twice a square.

  9 = 7 + 2×1^2
  15 = 7 + 2×2^2
  21 = 3 + 2×3^2
  25 = 7 + 2×3^2
  27 = 19 + 2×2^2
  33 = 31 + 2×1^2

  It turns out that the conjecture was false.

  What is the smallest odd composite that cannot be written as the 
  sum of a prime and twice a square?
  """


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils
-- import CLP.FD

check_a :: Int -> Bool
check_a n = if not $ null [(n,p,s) | p <- ps, s <- squares, n == p + s*2] then True else False
         where
           n2 = n `div` 2
           ps = take n2 primes
           squares = [i*i | i <- [1..n2]]

euler46a :: Int
euler46a = head $ filter (\n -> check_a n == False) $ cs
           where
           cs = filter (\n -> not $ isPrime n) [3,5..]

--                
-- reversing the order of primes and squares: Faster
--
check_b :: Int -> Bool
check_b n = if not $ null [(n,p,s) | s <- squares, p <- ps, n == p + s*2] then True else False
         where
           n2 = n `div` 2
           ps = take n2 primes
           squares = [i*i | i <- [1..n2]]

euler46b :: Int
euler46b = head $ filter (\n -> check_b n == False) $ cs
           where
           cs = filter (\n -> not $ isPrime n) [3,5..]

  --
-- Testing a smaller range
--
check_c :: Int -> Bool
check_c n = if not $ null ( take 1 [(n,p,s) | p <- ps, s <- squares, n == p + s*2]) then True else False
         where
           nd = n `div` 2
           ns = ceiling (sqrt (fromIntegral n))
           ps = take nd primes
           squares = [i*i | i <- [1..ns]]

euler46c :: Int
euler46c = head $ filter (\n -> check_c n == False) $ cs
           where
           cs = filter (\n -> not $ isPrime n) [3,5..]

--
-- Another approach: Much faster
-- (This is a port of my imperative Picat program euler46/0 in euler46.pi)
--
check_d :: Int -> Int -> Bool
check_d i j
  | j > round (sqrt ( (fromIntegral i) / 2 )) = True
  | isPrime (abs(i - (2*j*j)))                = False
  | otherwise                                 = check_d i (j+1)

euler46d' :: Int -> Int
euler46d' i
  | (not $ isPrime i) && check_d i 1  = i
  | otherwise                         = euler46d' (i+2)

euler46d :: Int
euler46d = euler46d' 3
  


main :: IO ()
main = do
         -- print euler46a -- (67.49 secs, 45,807,022,376 bytes)

         -- print euler46b -- (6.33 secs, 3,951,858,520 bytes)

         -- print euler46c -- (2.57 secs, 1,539,966,088 bytes)

         print euler46d -- (0.09 secs, 59,927,360 bytes)