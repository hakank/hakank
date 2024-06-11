{- 
  
  Euler #36 in Curry

  Problem 36
  """
  The decimal number, 585 = 1001001001_(2) (binary), is palindromic 
  in both bases.
  
  Find the sum of all numbers, less than one million, which are palindromic 
  in base 10 and base 2.

  (Please note that the palindromic number, in either base, may not 
   include leading zeros.)
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils
-- import CLP.FD


euler36a :: Int
euler36a = sum [n | n <- [0..999999], palin $ show n, palin $decToBase n 2]

-- using decToBase2 instead
euler36b :: Int
euler36b = sum [n | n <- [0..999999], palin $ show n, palin $decToBase2 n 2]


-- reverse dec and binary checks
-- slower
euler36c :: Int
euler36c = sum [n | n <- [0..999999], palin $decToBase n 2, palin $ show n]


main :: IO ()
main = do
         -- PAKCS: Execution time: 21746 msec. / elapsed: 28417 msec.
         -- KICS2: GHC compilation time: 1.29s / elapsed: 0:01.69 Execution time: 0.40s / elapsed: 0:00.42
         -- Curry2Go: Compilation time: 2.04s / elapsed: 0:01.50 Execution time: 200.37s / elapsed: 1:00.95
         print euler36a

         -- PAKCS: Execution time: 22911 msec. / elapsed: 29804 msec.
         -- KICS2: GHC compilation time: 1.31s / elapsed: 0:01.71 Execution time: 0.41s / elapsed: 0:00.43
         -- Curry2Go: Compilation time: 2.36s / elapsed: 0:01.60 Execution time: 199.16s / elapsed: 1:00.89
         -- print euler36b


         -- PAKCS: Execution time: 139915 msec. / elapsed: 159971 msec.
         -- KICS2: KiCS2 compilation time: 1.67s / elapsed: 0:02.08 GHC compilation time: 1.28s / elapsed: 0:01.64 Execution time: 1.70s / elapsed: 0:01.71
         -- Curry2Go: Compilation time: 2.16s / elapsed: 0:01.55 Execution time: 1477.29s / elapsed: 7:25.92
         -- print euler36c
