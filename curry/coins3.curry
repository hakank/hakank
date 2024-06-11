{- 
  
  Coins puzzle in Curry CLP.FD

  From "The ECLiPSe Book" pages 99f and 234 ff
  The solution in ECLiPSe is at page 236.

  """
  What is the minimum number of coins that allows one to pay _exactly_
  any amount smaller than one Euro? Recall that there are six different
  euro cents, of denomination 1, 2, 5, 10, 20, 50
  """

  There are 4 optimal solutions (8 coins):
    x=[1,2,1,2,1,1]
    x=[1,2,2,1,1,1]
    x=[2,1,1,2,1,1]
    x=[2,1,2,1,1,1]

  This is a port of my Picat model http://hakank.org/picat/coins3.pi

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import CLP.FD
import Debug.Trace

coins3 :: [Int]
coins3 = let
           variables = [1,2,5,10,25,50] -- the different coin values
           n = length variables
           xs = take n (domain 0 99)
           -- total number of coins used (to minimize)
           numCoins = head (domain 0 99)
           numCoinsOpt = anyOf [0..10] -- for optimization
         in
           solveFDOne [FirstFail,Bisect,Minimize numCoinsOpt] (numCoins:xs) $
           fd numCoinsOpt =# numCoins /\           
           numCoins =# Data.List.sum xs /\
           foldl1 (/\) [ scalarProduct variables tmp Equ (fd j) /\
                         foldl1 (/\) [ tmp !! i <=# xs !! i  | i <- [0..n-1] ]
                        |  j <- [0..99],
                           let tmp = take n (domain 0 99)
                       ]

main :: ([Int],[Int])
main =  splitAt 1 $ coins3

