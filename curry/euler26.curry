{- 
  
  Euler #26 in Curry

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


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
import Control.SetFunctions
import HakankUtils
-- import CLP.FD



--
-- Get the length of the repeating cycle for 1/n
--
-- Prelude Data.List> _ ++ [(1,x )] ++ _ =:= (zip [10,15,14,4,6,9,5,16,7,2,3,13,11,8,12,1,10] [1..17]) where x free
-- {x = 16} True
getPos :: (Prelude.Eq a, Prelude.Num b) => a -> b -> [a] -> b
getPos val pos (x:xs) 
    | null xs = pos
    | x == val = pos
    | otherwise = getPos val (pos+1) xs

getRepLen :: Int -> Int
getRepLen n = 1+(getPos 1 0 [(10^i) `mod` n | i <- [1..n]])

-- Free variables
getRepLen2 :: (Data a, Integral a) => a -> a
getRepLen2 n = minValue $ set0 (i =:= anyOf [1..n] &> 10^i `mod` n =:= 1 &> i) where i free

getRepLen3_ :: Int -> Int -> Int
getRepLen3_ n i
          | i == n = 0
          | 10^i `mod` n == 1 = i
          | otherwise         = getRepLen3_ n (i+1)

getRepLen3 :: Int -> Int
getRepLen3 n = getRepLen3_ n 1



-- This works only for n >= 11
euler26a :: Int
euler26a = snd $ last $ sort $ [(getRepLen n,n) | n <- [2..999], isPrime n]

-- Variant using maximum instead
euler26b :: Int
euler26b = snd $ maximum $ [(getRepLen n,n) | n <- [2..999], isPrime n]

-- Free variables: much slower
euler26c :: Int
euler26c = snd $ maximum $ [(getRepLen2 n,n) | n <- [11..999], isPrime n]

euler26d :: Int
euler26d = snd $ maximum [(getRepLen3 n,n) | n <- [11..999], isPrime n]

euler26e :: Int
euler26e = snd $ maximum [ (head [i | i <- [1..n], 1==(10^i) `mod` n], n) | n <- [11..999], isPrime n]

main :: IO ()
main = do
         -- PAKCS: Execution time: 6515 msec. / elapsed: 7540 msec.
         -- KICS2: KiCS2 compilation time: 1.65s / elapsed: 0:02.02 GHC compilation time: 1.63s / elapsed: 0:02.03 Execution time: 0.15s / elapsed: 0:00.16
         -- Curry2Go: Cannot handle large integers
         -- print euler26a


         -- PAKCS: Execution time: 6654 msec. / elapsed: 7655 msec.
         -- KICS2: KiCS2 compilation time: 2.05s / elapsed: 0:02.44 GHC compilation time: 1.65s / elapsed: 0:02.07 Execution time: 0.14s / elapsed: 0:00.15
         -- Curry2Go: Cannot handle large integers
         -- print euler26b

         -- PAKCS: Does not work in PAKCS: suspended constraints
         -- KICS2: KiCS2 compilation time: 2.31s / elapsed: 0:02.64 GHC compilation time: 1.87s / elapsed: 0:02.35 Execution time: 433.99s / elapsed: 7:49.58
         -- Curry2Go: Cannot handle larger integers
         -- print euler26c

         -- PAKCS: Execution time: 6747 msec. / elapsed: 7749 msec.
         -- KICS2: KiCS2 compilation time: 1.92s / elapsed: 0:02.28 GHC compilation time: 1.29s / elapsed: 0:01.70 Execution time: 0.13s / elapsed: 0:00.14
         -- Curry2Go: Cannot handle larger integers
         print euler26d

         -- PAKCS: Execution time: 6284 msec. / elapsed: 7287 msec.
         -- KICS2: KiCS2 compilation time: 1.88s / elapsed: 0:02.29 GHC compilation time: 1.31s / elapsed: 0:01.71 Execution time: 0.15s / elapsed: 0:00.16
         -- Curry2Go: -
         -- print euler26e