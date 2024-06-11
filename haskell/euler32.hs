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

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils
-- import CLP.FD

--
-- Return a slice of list a from position from to position to
--
-- range :: [a] -> Int -> Int -> [a]
-- range a from to = [a !! i | i <- [from..to]]

check_a' :: [Char] -> Int -> Int
check_a' p pos = if p1*p2 == p3 then p3  else 0
                 where
                   p1 = strToNum $ range p 0 pos
                   p2 = strToNum $ range p (pos+1) 4
                   p3 = strToNum $ range p 5 8

check_a :: [Char] -> [Int]
check_a p =  [pr | pos <- [0..3], pr <- [check_a' p pos], pr > 0] 


-- euler32b :: Int
euler32a = (sum . nub . concat) $ filter (/= []) $ map check_a $ (permutations "123456789")

-- euler32b is a Curry specific version of euler32a
           
euler32c :: Int
euler32c = sum $ nub $ [a*b | a <- [2..98], b <- [a+1..9876], t <- [(show a) ++ (show b) ++ (show (a*b))], length t == 9, not (elem '0' t), length (nub t) == 9]

           
main = do
         -- print euler32a -- (19.35 secs, 20,875,174,704 bytes)

         print euler32c -- (0.90 secs, 1,346,132,840 bytes)