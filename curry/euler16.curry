{- 
  
  Euler #16 in Curry

  Problem 16
  """
  2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
  
  What is the sum of the digits of the number 2^1000?
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Data.Char
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
-- import CLP.FD

euler16a :: Int
euler16a = sum $ map digitToInt $ show $ 2^1000

main :: IO ()
main = do
         -- PAKCS: Execution time: 10 msec. / elapsed: 15 msec. 
         -- KICS2: 1.16s 1.20s 0.00s
         -- Curry2Go: Cannot handle large integers: 2^100 -> 0 !!
         print euler16a

