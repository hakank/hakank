{- 
  
  Subset sum problem in Curry CLP.FD

  From Katta G. Murty: "Optimization Models for Decision Making", page 340
  http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
  
  """
  Example 7.8.1
  
  A bank van had several bags of coins, each containing either
  16, 17, 23, 24, 39, or 40 coins. While the van was parked on the
  street, thieves stole some bags. A total of 100 coins were lost.
  It is required to find how many bags were stolen.
  """

  This is a port of my Picat model http://hakank.org/picat/subset_sum.pi

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import CLP.FD


subsetSum :: [Int] -> Int -> [Int]
subsetSum coins total = let
                          n = length coins
                          xs = take n (domain 0 n)
                          numStolen = head (domain 0 (Data.List.sum coins))
                        in
                          solveFD [] (numStolen:xs) $
                          numStolen =# Data.List.sum xs /\
                          scalarProduct (map fd coins) xs Equ (fd total)

{-
  6 bags were stolen: 2 of 16 coins and 4 och 17 coins:


  Execution time: 29 msec. / elapsed: 29 msec.
-}
main :: (Int,[Int])
main = (numStolen,xs)
       where
         coins = [16, 17, 23, 24, 39, 40]
         total = 100
         (numStolen:xs) = subsetSum coins total 