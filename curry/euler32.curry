{- 
  
  Euler #32 in Curry

  Problem 32
  """
  We shall say that an n-digit number is pandigital if it makes use of 
  all the digits 1 to n exactly once; for example, the 5-digit number, 
  15234, is 1 through 5 pandigital.

  The product 7254 is unusual, as the identity, 39 × 186 = 7254, 
  containing multiplicand, multiplier, and product is 1 through 9 
  pandigital.

  Find the sum of all products whose multiplicand/multiplier/product 
  identity can be written as a 1 through 9 pandigital.
  HINT: Some products can be obtained in more than one way so be sure 
  to only include it once in your sum.
  """

  See euler32_clp.curry for a CLP.FD version (only for PAKCS).
  It's quite faster: Execution time: 99 msec. / elapsed: 98 msec.

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Control.AllValues
import Control.SetFunctions
import HakankUtils
-- import CLP.FD

--
-- Return a slice of list a from position from to position to
--
range :: [a] -> Int -> Int -> [a]
range a from to = [a !! i | i <- [from..to]]

--
-- Check is p[0..pos] * p[pos1..4] = p[5..8] for pos in 0..3
--
check_a' :: [Char] -> Int -> Int
check_a' p pos = if p1*p2 == p3 then p3  else 0
                 where
                   p1 = strToNum $ range p 0 pos
                   p2 = strToNum $ range p (pos+1) 4
                   p3 = strToNum $ range p 5 8

--
-- Check the (four) possible combinations to ge a pandigital number.
-- 
--
check_a :: [Char] -> [Int]
check_a p =  [pr | pos <- [0..3], pr <- [check_a' p pos], pr > 0] 

euler32a :: Int
euler32a = (sum . nub . concat) $ filter (/= []) $ map check_a $ (permutations "123456789")


--
-- Using nondeterminism instead.
-- Much slower.
check_b :: [Char] -> Int
check_b p = if p1*p2 == p3 then p3 else 0
            where
              pos = anyOf [0..3] -- position is non deterministic 
              p1 = strToNum $ range p 0 pos
              p2 = strToNum $ range p (pos+1) 4
              p3 = strToNum $ range p 5 8

euler32b :: Int
euler32b = (sum . nub . concat) $ filter (/= []) $ map (\p -> allValues $ check_b p) $ (permutations "123456789")

--
-- One-liner list comprehension: much faster
--
euler32c :: Int
euler32c = sum $ nub $ [a*b | a <- [2..98], b <- [a+1..9876], t <- [(show a) ++ (show b) ++ (show (a*b))], length t == 9, not (elem '0' t), length (nub t) == 9]


main :: IO ()
main = do
          -- PAKCS: Execution time: 880779 msec. / elapsed: 1010712 msec.
          -- KICS2: KiCS2 compilation time: 1.71s / elapsed: 0:02.02 GHC compilation time: 1.29s / elapsed: 0:01.63 Execution time: 14.95s / elapsed: 0:14.99
          -- Curry2Go: Compilation time: 1.96s / elapsed: 0:01.64 Execution time: 2824.21s / elapsed: 29:11.12
          -- print euler32a

          -- PAKCS: Execution time: 980348 msec. / elapsed: 986905 msec.
          -- KICS2: KiCS2 compilation time: 1.74s / elapsed: 0:02.06 GHC compilation time: 1.30s / elapsed: 0:01.76 Execution time: 239.11s / elapsed: 4:00.55
          -- Curry2Go: Compilation time: 2.04s / elapsed: 0:01.74 Execution time: 4469.37s / elapsed: 21:00.86
          -- print euler32b

          -- PAKCS: Execution time: 51386 msec. / elapsed: 72463 msec.
          -- KICS2: KiCS2 compilation time: 1.74s / elapsed: 0:02.15 GHC compilation time: 1.33s / elapsed: 0:01.78 Execution time: 0.95s / elapsed: 0:00.96
          -- Curry2Go:Compilation time: 2.30s / elapsed: 0:01.61 Execution time: 160.02s / elapsed: 1:23.66
          print euler32c