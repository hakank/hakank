--
--   Euler #1 in Haskell.
--
--   Problem 1
--   """
--   If we list all the natural numbers below 10 that are multiples of 3 or 5, 
--   we get 3, 5, 6 and 9. The sum of these multiples is 23.
--   Find the sum of all the multiples of 3 or 5 below 1000.
--   """
--
--   This Haskell model was created by Hakan Kjellerstrand, hakank@gmail.com
--   See also my Haskell page: http://www.hakank.org/haskell/
--
-- 
module Main where

f n m = [i | i <- [1..n], i `mod` m == 0]
f2 n m1 m2 = [i | i <- [1..n], (mod i m1 == 0) || (mod i m2 == 0)]

euler1a = sum [i | i <- [1..999], (mod i 3 == 0) || (mod i 5 == 0)]
euler1b = sum([i | i <- [1..999], mod i 3 == 0]) + sum([i | i <- [1..999], mod i 5 == 0]) - sum([i | i <- [1..999], mod i 3 == 0 && mod i 5 ==0])

euler1c = sum (f2 999 3 5)

main = do
    print euler1a
    print euler1b
    print euler1c

