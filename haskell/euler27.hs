{- 
  
  Euler #27 in Haskell

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
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
import HakankUtils


p27 a b = n 
           where
            n1 = 0
            pp = n1^2 + a*n1 + b
            n = if pp > 1 then p27' pp a b n1 else 0

p27' pp a b n0 
     | pp <= 1 || not (isPrime pp) = n0
     | otherwise = n2
       where 
         n1 = n0+1
         pp1 = n1^2 + a*n1 + b
         n2 = p27' pp1 a b n1


euler27a = last $ sort [ (len,a*b,a,b) | a <- [-t..t], b <- [-t..t], len <- [p27 a b]]           
         where
            t = 999

-- faster
euler27b = maximum [ (len,a*b,a,b) | a <- [-t..t], b <- [-t..t], len <- [p27 a b]]
           -- last $ sort [ (len,a*b,a,b) | a <- [-t..t], b <- [-t..t], len <- [p27 a b]]           
         where
            t = 999
                
main = do
         print euler27a -- (23.26 secs, 15,997,621,120 bytes)
         print euler27b -- (14.85 secs, 11,048,451,896 bytes)