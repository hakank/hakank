{- 
  
  Euler #36 in Haskell

  Problem 36
  """
  The decimal number, 585 = 1001001001_(2) (binary), is palindromic 
  in both bases.
  
  Find the sum of all numbers, less than one million, which are palindromic 
  in base 10 and base 2.

  (Please note that the palindromic number, in either base, may not 
   include leading zeros.)
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
import HakankUtils

euler36a :: Int
euler36a = sum [n | n <- [0..999999], palin $ show n, palin $decToBase n 2]

-- using decToBase2 instead
euler36b :: Int
euler36b = sum [n | n <- [0..999999], palin $ show n, palin $decToBase2 n 2]


-- reverse dec and binary checks
-- slower
euler36c :: Int
euler36c = sum [n | n <- [0..999999], palin $decToBase n 2, palin $ show n]


main :: IO ()
main = do
         print euler36a -- (0.63 secs, 749,909,120 bytes)

         -- print euler36b -- (0.67 secs, 764,536,296 bytes)

         -- print euler36c -- (7.14 secs, 7,240,519,688 bytes)
