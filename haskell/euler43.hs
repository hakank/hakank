{- 
  
  Euler #43 in Haskell

  """  
  The number, 1406357289, is a 0 to 9 pandigital number because it is made up of 
  each of the digits 0 to 9 in some order, but it also has a rather interesting 
  sub-string divisibility property.
  
  Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we 
  note the following:
  
      * d2d3d4=406 is divisible by 2
      * d3d4d5=063 is divisible by 3
      * d4d5d6=635 is divisible by 5
      * d5d6d7=357 is divisible by 7
      * d6d7d8=572 is divisible by 11
      * d7d8d9=728 is divisible by 13
      * d8d9d10=289 is divisible by 17
  
  Find the sum of all 0 to 9 pandigital numbers with this property.
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
import HakankUtils (digitsToNum,runningChunksOf)

getIxes :: [Int] -> [Int] -> [Int]
getIxes lst ixs = [lst !! i | i <- ixs]

checkPerm :: [Int] -> Bool
checkPerm ps = all (==0) $ zipWith (mod) ds [2,3,5,7,11,13,17]
            where
              ds = [digitsToNum $ getIxes ps ix | ix <- runningChunksOf 3 [1..(length ps)-1] ]

euler43a :: Int
euler43a = sum $ map digitsToNum $ filter  checkPerm $ permutations [0..9]

main :: IO ()
main = do
         print euler43a -- (35.03 secs, 37,448,117,704 bytes)

