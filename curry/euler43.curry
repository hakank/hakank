{- 
  
  Euler #43 in Curry

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

  Also see euler43_clp.curry (only for PAKCS) which is fast (much faster than 
  any of the non CLP.FD here):
  - Execution time: 24 msec. / elapsed: 24 msec.

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (digitsToNum,runningChunksOf)
-- import CLP.FD

--
-- getIxes lst ixs
-- Extract the elements in lst given the indices in ixs
--
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
         -- PAKCS: Execution time: 1202086 msec. / elapsed: 1402216 msec.
         -- KICS2: KiCS2 compilation time: 1.67s / elapsed: 0:02.05 GHC compilation time: 1.61s / elapsed: 0:02.05 Execution time: 20.56s / elapsed: 0:20.61
         -- Curry2Go: Compilation time: 2.37s / elapsed: 0:01.63 Execution time: 4144.34s / elapsed: 42:21.65
         print euler43a
         