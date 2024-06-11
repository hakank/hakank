{- 
  
  Euler #23 in Haskell

  """
  A perfect number is a number for which the sum of its proper divisors 
  is exactly equal to the number. For example, the sum of the proper divisors 
  of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

  A number n is called deficient if the sum of its proper divisors is less than 
  n and it is called abundant if this sum exceeds n.

  As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number 
  that can be written as the sum of two abundant numbers is 24. By mathematical 
  analysis, it can be shown that all integers greater than 28123 can be written 
  as the sum of two abundant numbers. However, this upper limit cannot be reduced 
  any further by analysis even though it is known that the greatest number that 
  cannot be expressed as the sum of two abundant numbers is less than this limit.

  Find the sum of all the positive integers which cannot be written as the sum of 
  two abundant numbers.
  """ 


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
import qualified Data.Set as Set
import HakankUtils

abundant :: Int -> Bool
abundant n = sumDivisors n > n

-- cross x y =  pure (,) <*> x <*> y

-- Brute force
euler23a =   sum $ [1..n] \\ nub [x+y | x <- ablist, y <- ablist, x <= y, x + y <= n]
             where
             n = 28123
             ablist = filter abundant [1..n]

-- Smaller limit
-- From http://mathworld.wolfram.com/AbundantNumber.html: 
-- "Every number greater than 20161 can be expressed as a sum of two abundant numbers."
euler23b =   sum $ [1..n] \\ nub [x+y | x <- ablist, y <- ablist, x <= y, x + y <= n]
             where
             n = 20161
             ablist = filter abundant [1..n]
  

-- another approach
euler23e' ab n aux
   | n > 20162 = aux
   | otherwise = euler23e' ab' (n+1) aux'
                 where
                    ab' = if abundant n then n : ab else ab
                    aux' = if not $ or [ (n-a) `elem` ab' | a <- ab'] then aux+n else aux

euler23e :: Int
euler23e = euler23e' [] 1 0
    
--
-- Using Data.Set
--
euler23f' ab n aux
   | n > 20162 = aux
   | otherwise = euler23f' ab' (n+1) aux'
                 where
                    ab' = if abundant n then Set.insert n ab else ab
                    aux' = if not $ or [ Set.member na ab | a <- Set.toList ab, let na = n-a] then aux+n else aux

-- euler23f :: Int
euler23f = euler23f' Set.empty 1 0


main = do
         -- print euler23a -- (1288.86 secs, 29,492,417,208 bytes)

         -- print euler23b -- (443.97 secs, 15,464,072,736 bytes)

         -- print euler23e -- (126.28 secs, 2,857,730,616 bytes)
  
         print euler23f -- (3.54 secs, 3,014,560,384 bytes)

