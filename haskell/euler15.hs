{- 
  
  Euler #15 in Haskell

  Problem 15
  """
  Starting in the top left corner of a 2×2 grid, there are 6 routes 
  (without backtracking) to the bottom right corner.
  
  How many routes are there through a 20×20 grid?
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
-- import HakankUtils

euler15a = product [21..40] `div` product [2..20]


main = do
         print euler15a -- (0.00 secs, 90,056 bytes)
