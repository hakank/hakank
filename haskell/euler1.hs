{-

  Euler #1 in Haskell.

  Problem 1
  """
  If we list all the natural numbers below 10 that are multiples of 3 or 5, 
  we get 3, 5, 6 and 9. The sum of these multiples is 23.
  Find the sum of all the multiples of 3 or 5 below 1000.
  """

  This Haskell program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Haskell page: http://www.hakank.org/haskell/


-}

import Data.List
import System.CPUTime

euler1a = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

euler1b = sum $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1..999]

mod_3_or_5 x = x `mod` 3 == 0 || x `mod` 5 == 0
euler1c = sum $ filter mod_3_or_5 [1..999]

euler1d = sum $ union [3,6..999] [5,10..999]

euler1e =  (sum . nub) $ [3,6..999] ++ [5,10..999]

euler1g = sum([i | i <- [1..999], mod i 3 == 0]) + sum([i | i <- [1..999], mod i 5 == 0]) - sum([i | i <- [1..999], mod i 3 == 0 && mod i 5 ==0])


main = do
         print euler1a -- (0.01 secs, 477,856 bytes)
         -- print euler1b -- (0.01 secs, 427,384 bytes)
         -- print euler1c -- (0.01 secs, 859,152 bytes)
         -- print euler1d -- (0.01 secs, 3,305,176 bytes)
         -- print euler1e -- (0.01 secs, 209,208 bytes)
         -- print euler1g -- (0.01 secs, 862,560 bytes)
     
