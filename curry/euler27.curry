{- 
  
  Euler #27 in Curry

  """
  Euler published the remarkable quadratic formula:

  n^2 + n + 41

  It turns out that the formula will produce 40 primes for the consecutive values 
  n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 
  41, and certainly when n = 41, 41^2 + 41 + 41 is clearly divisible by 41.

  Using computers, the incredible formula  n^2 − 79n + 1601 was discovered, which 
  produces 80 primes for the consecutive values n = 0 to 79. The product of the 
  coefficients, −79 and 1601, is −126479.

  Considering quadratics of the form:

      n^2 + an + b, where |a| < 1000 and |b| < 1000

      where |n| is the modulus/absolute value of n
      e.g. |11| = 11 and |−4| = 4

  Find the product of the coefficients, a and b, for the quadratic 
  expression that produces the maximum number of primes for consecutive 
  values of n, starting with n = 0.
  """ 

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils
-- import CLP.FD

p27 :: Int -> Int -> Int
p27 a b = n 
          where
           n1 = 0
           pp = n1^2 + a*n1 + b
           n = if pp > 1 then p27' pp a b n1 else 0

p27' :: Int -> Int -> Int -> Int -> Int
p27' pp a b n0 
            | pp <= 1 || not (isPrime pp) = n0
            | otherwise = n2
              where 
                n1 = n0+1
                pp1 = n1^2 + a*n1 + b
                n2 = p27' pp1 a b n1

euler27a :: (Int,Int)
euler27a = last $ sort [ (len,a*b) | a <- [-t..t], b <- [-t..t], len <- [p27 a b]]
           where
             t = 999

-- Faster
euler27b :: (Int,Int)
euler27b = maximum [ (len,a*b) | a <- [-t..t], b <- [-t..t], len <- [p27 a b]]
           where
             t = 999

main :: IO ()
main = do
        -- PAKCS: >10min
        -- KICS2: KiCS2 compilation time: 1.75s / elapsed: 0:02.11 GHC compilation time: 1.40s / elapsed: 0:01.78 Execution time: 36.27s / elapsed: 0:37.22
        -- Curry2Go: >10min
        -- print euler27a


        -- PAKCS: > 10min
        -- KICS2: KiCS2 compilation time: 1.68s / elapsed: 0:02.10 GHC compilation time: 1.31s / elapsed: 0:01.73 Execution time: 8.36s / elapsed: 0:09.05
        -- Curry2Go: >10min
        print euler27b