{- 
  
  Euler #48 in Curry

  """
  The series, 1^(1) + 2^(2) + 3^(3) + ... + 10^(10) = 10405071317.
  
  Find the last ten digits of the series, 
  1^(1) + 2^(2) + 3^(3) + ... + 1000^(1000).
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils
-- import CLP.FD

euler48a :: [Int]
euler48a = drop (len-10) xs
           where
           xs = numToDigits $ sum [i^i | i <- [1..1000]]
           len = length xs

main :: IO ()
main = do
         -- PAKCS: Execution time: 229 msec. / elapsed: 241 msec.
         -- KICS2: KiCS2 compilation time: 1.62s / elapsed: 0:02.00 GHC compilation time: 1.28s / elapsed: 0:01.68 Execution time: 0.01s / elapsed: 0:00.02
         -- Curry2Go: Cannot handle large numbers
         print euler48a