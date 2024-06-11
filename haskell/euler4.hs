{- 
  
  Euler #4 in Haskell

  Problem 4
  """
  A palindromic number reads the same both ways. The largest palindrome made 
  from the product of two 2-digit numbers is 9009 = 91 × 99.

  Find the largest palindrome made from the product of two 3-digit numbers.
  """


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
   
-}

import Data.List
import HakankUtils

-- palin xs = xs == reverse xs

euler4a = maximum [x*y | x <- [100..999], y <- [x..999], palin (show (x*y))]
euler4b = maximum [t | x <- [100..999], y <- [x..999], t <- [x*y], palin (show t)]

-- Using Applicative Function for cross product: Slower
euler4c = maximum $ filter (\x -> palin $ show x) ( pure (*) <*> [100..999] <*> [100..999])

main = do
         print euler4a -- (0.29 secs, 317,132,720 bytes)
         -- print euler4b -- (0.34 secs, 359,220,224 bytes)
         -- print euler4c -- (0.44 secs, 581,379,552 bytes)

         
