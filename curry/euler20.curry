{- 
  
  Euler #20 in Curry

  Problem 20
  """
  n! means n (n 1) ... 3 2 1

  Find the sum of the digits in the number 100!")
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Data.Char
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils
-- import CLP.FD

euler20a :: Int 
euler20a = sum $ map (digitToInt) $ show $ fact 100

euler20b :: Int 
euler20b = sum $ numToDigits $ fact 100

euler20c :: Int 
euler20c = sum $ numToDigits $ foldr1 (*) [1..100]

main :: IO ()
main = do
         -- PAKCS: Execution time: 7 msec. / elapsed: 6 msec. 
         -- KICS2: 1.14s 1.19s 0.00s
         -- Curry2Go: Cannot handle 100!
         -- print euler20a
         
         -- PAKCS: Execution time: 2 msec. / elapsed: 4 msec. 
         -- KICS2: 1.72s 2.69s 0.00s
         -- Curry2Go: Cannot handle 100!     
         print euler20b
         
         -- PAKCS: Execution time: 6 msec. / elapsed: 6 msec. 
         -- KICS2: 1.71s 1.25s 0.00s  
         -- Curry2Go: Cannot handle 100!
         -- print euler20c

