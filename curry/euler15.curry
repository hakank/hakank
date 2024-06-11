{- 
  
  Euler #15 in Curry

  Problem 15
  """
  Starting in the top left corner of a 2×2 grid, there are 6 routes 
  (without backtracking) to the bottom right corner.
  
  How many routes are there through a 20×20 grid?
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
-- import CLP.FD

euler15a :: Int
euler15a = product [21..40] `div` product [2..20]

main :: IO ()
main = do
         -- PAKCS: Execution time: 1 msec. / elapsed: 1 msec. 
         -- KICS2: 1.10s 1.26s 0.00s
         -- Curry2Go:  Stopped after 55 minutes
         print euler15a
