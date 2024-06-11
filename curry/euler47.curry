{- 
  
  Euler #47 in Curry

  """  
  The first two consecutive numbers to have two distinct prime factors are:

  14 = 2 x 7
  15 = 3 x 5

  The first three consecutive numbers to have three distinct 
  prime factors are:

  644 = 2^2 x 7 x 23
  645 = 3 x 5 x 43
  646 = 2 x 17 x 19.

  Find the first four consecutive integers to have four distinct primes 
  factors. What is the first of these numbers?
  """ 


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Control.AllValues
-- import Control.SetFunctions
import HakankUtils
-- import CLP.FD

numPrimeFactors :: Int -> Int
numPrimeFactors n = length $ nub $ primeFactors n


--
-- First version.
--
euler47a' :: (Prelude.Eq b, Prelude.Num b, Prelude.Num c) => [(a, b)] -> c -> c
euler47a' xs i
  | c1 == 4 && c2 == 4 && c3 == 4 && c4 == 4 = i
  | otherwise                                = euler47a' (tail xs) (i+1)
  where
  [(_,c1),(_,c2),(_,c3),(_,c4)] = take 4 xs  

euler47a :: Prelude.Num a => a
euler47a = euler47a' (map (\n -> (n, numPrimeFactors n)) [1..]) 1

--
-- Simpler, but not faster
--
euler47b' :: (Eq a, Num a, Num b) => [a] ->  b -> b
 
euler47b' xs i
  | c1 == 4 && c2 == 4 && c3 == 4 && c4 == 4 = i
  | otherwise                                = euler47b' (tail xs) (i+1)
  where
  [c1,c2,c3,c4] = take 4 xs

euler47b :: Prelude.Num a => a
euler47b = euler47b' (map numPrimeFactors [1..]) 1

--
-- Even simpler and not faster
--
euler47c' :: (Eq a, Num a, Num b) => [a] ->  b -> b
euler47c' xs i
  | [4,4,4,4] == take 4 xs = i
  | otherwise              = euler47c' (tail xs) (i+1)

euler47c :: Prelude.Num a => a
euler47c = euler47c' (map numPrimeFactors [1..]) 1


--
-- Here's an idea using free variables.
-- It would only work in KiCS2 (PAKCS and Curry2Go yield "suspended constraints"),
-- and only works for [2,2] and [3,3,3].
-- For [4,4,4,4] the memory is exhausted..
--
-- Here's for [2,2] with a list [1..100]
--  > euler47x
--  Just 14
--  2.09s + 8.34s + 0.02s
-- 
-- Here's for [3,3,3] with a list [1..1000]
--  > euler47x
--  (Just 644)
--  1.88s + 3.66s + 0.15s
--
-- For [4,4,4,4] with a list [1..200000]
--  > euler47x
--  Memory exhausted
--
-- It seems that this list is not handled lazily when using  .. ++ _ =:= ...
-- so we have to use an non infinite list
euler47x' :: Prelude.Int -> [Prelude.Int]
euler47x' n = map numPrimeFactors [1..n]
euler47x :: Prelude.Maybe Prelude.Int
-- euler47x = oneValue $ x ++ [2,2] ++ _ =:= (euler47x' 100) &> 1+(length x) where x free -- ok
euler47x = oneValue $ x ++ [3,3,3] ++ _ =:= (euler47x' 1000) &> 1+(length x) where x free -- ok 
-- euler47x = oneValue $ x ++ [4,4,4,4] ++ _ =:= (euler47x' 200000)  &> 1+(length x) where x free -- Memory exhausted


main :: IO ()
main = do
         -- PAKCS: Execution time: 855736 msec. / elapsed: 979956 msec
         -- KICS2: KiCS2 compilation time: 1.69s / elapsed: 0:02.06 GHC compilation time: 1.24s / elapsed: 0:01.64 Execution time: 12.66s / elapsed: 0:12.69
         -- Curry2Go: >60min
         print euler47a

         -- PAKCS: -
         -- KICS2: KiCS2 compilation time: 1.68s / elapsed: 0:02.03 GHC compilation time: 3.42s / elapsed: 0:03.90 Execution time: 13.18s / elapsed: 0:13.21
         -- Curry2Go: -
         -- print euler47b


         -- PAKCS:Execution time: 869761 msec. / elapsed: 998436 msec.
         -- KICS2: KiCS2 compilation time: 1.64s / elapsed: 0:02.02 GHC compilation time: 1.34s / elapsed: 0:01.73 Execution time: 13.69s / elapsed: 0:13.87
         -- Curry2Go: >60min
         -- print euler47c
