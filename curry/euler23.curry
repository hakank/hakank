{- 
  
  Euler #23 in Curry

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
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import qualified Data.Set as Set
import Data.Map
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (sumDivisors)
-- import CLP.FD


abundant :: Int -> Bool
abundant n = sumDivisors n > n


-- Brute force
euler23a :: Int
euler23a =   sum $ [1..n] \\ nub [x+y | x <- ablist, y <- ablist, x <= y, x + y <= n]
             where
             n = 28123
             ablist = filter abundant [1..n]-- GHCi:


-- Smaller limit
-- From http://mathworld.wolfram.com/AbundantNumber.html: 
-- "Every number greater than 20161 can be expressed as a sum of two abundant numbers."
euler23b :: Int
euler23b = sum $ [1..n] \\ nub [x+y | x <- ablist, y <- ablist, x <= y, x + y <= n]
           where
             n = 20161
             ablist = filter abundant [1..n]

euler23c :: Int
euler23c = sum(lst)
           where
             n = 20161
             is = [1..n]
             ablist = filter abundant is
             lst = is \\ nub [t | x <- ablist, y <- ablist, x <= y, t <-[x+y], t < n]

-- Does sorting before nub have any effect?
-- Yes, it's slower.
euler23d :: Int
euler23d = sum $ [1..n] \\ (nub $ sort [x+y | x <- ablist, y <- ablist, x <= y, x + y <= n])
           where
             n = 20161
             ablist = filter abundant [1..n]

--
-- Another approach, faster but still very slow.
--
euler23e' :: [Int] -> Int -> Int -> Int
euler23e' ab n aux
   | n > 20162 = aux
   | otherwise = euler23e' ab' (n+1) aux'
                 where
                    ab' = if abundant n then n : ab else ab
                    aux' = if not $ or [ (n-a) `elem` ab | a <- ab] then aux+n else aux

euler23e :: Int
euler23e = euler23e' [] 1 0


--
-- Using Data.Set
-- Much faster: 5.3s (KICS2)
--
euler23f' :: Data.Map.Map Int () -> Int -> Int -> Int 
euler23f' ab n aux
   | n > 20162 = aux
   | otherwise = euler23f' ab' (n+1) aux'
                 where
                    ab' = if abundant n then Set.insert n ab else ab
                    aux' = if not $ or [ Set.member (n-a) ab | a <- Set.toList ab] then aux+n else aux

euler23f :: Int
euler23f = euler23f' Set.empty 1 0



main :: IO ()
main = do
         -- PAKCS: > 7h
         -- KICS2: 1.32s 3813.17s (1h3min33.17s)
         -- Curry2Go: 
         -- print euler23a

         -- PAKCS: 
         -- KICS2: KiCS2 compilation time: 1.73s / elapsed: 0:02.11 GHC compilation time: 3.79s / elapsed: 0:04.26 Execution time: 1327.75s / elapsed: 22:08.84
         -- Curry2Go: > 70min (with 2 threads)
         -- print euler23b

         -- PAKCS:
         -- KICS2: KiCS2 compilation time: 1.58s / elapsed: 0:01.95 GHC compilation time: 1.45s / elapsed: 0:01.91 Execution time: 1438.53s / elapsed: 24:16.90
         -- Curry2Go
         -- print euler23c


         -- PAKCS:
         -- KICS2: KiCS2 compilation time: 1.64s / elapsed: 0:01.98 GHC compilation time: 1.39s / elapsed: 0:01.86 Execution time: 3021.89s / elapsed: 50:30.59
         -- Curry2Go
         -- print euler23d

         -- PAKCS:
         -- KICS2: KiCS2 compilation time: 1.94s / elapsed: 0:02.41 GHC compilation time: 1.44s / elapsed: 0:01.82 Execution time: 370.68s / elapsed: 6:11.93
         -- Curry2Go: 
         -- print euler23e


         -- PAKCS: Execution time: 317221 msec. / elapsed: 366554 msec.
         -- KICS2: KiCS2 compilation time: 1.95s / elapsed: 0:02.36 GHC compilation time: 1.50s / elapsed: 0:01.95 Execution time: 5.30s / elapsed: 0:05.31 
         -- Curry2Go: Compilation time: 2.55s / elapsed: 0:01.91 Execution time: 3067.66s / elapsed: 14:57.01
         print euler23f