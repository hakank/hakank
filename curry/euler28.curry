{- 
  
  Euler #28 in Curry

  Problem 28
  """
  Starting with the number 1 and moving to the right in a clockwise 
  direction a 5 by 5 spiral is formed as follows:
  
     21 22 23 24 25
     20  7  8  9 10
     19  6  1  2 11
     18  5  4  3 12
     17 16 15 14 13

  It can be verified that the sum of the numbers on the diagonals is 101.
  
  What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the 
  same way?
  """


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
-- import CLP.FD

-- Note the parenthesis...
euler28a :: Int
euler28a = 1 + (sum [ 4*(n^2) - (6*n) + 6 | n <- [3,5..1001]])

main :: IO ()
main = do
        -- PAKCS: Execution time: 47 msec. / elapsed: 55 msec.
        -- KICS2: KiCS2 compilation time: 1.23s / elapsed: 0:01.39 GHC compilation time: 1.33s / elapsed: 0:01.68 Execution time: 0.00s / elapsed: 0:00.01
        -- Curry2Go: Compilation time: 1.74s / elapsed: 0:01.33 Execution time: 0.13s / elapsed: 0:00.11
        print euler28a
