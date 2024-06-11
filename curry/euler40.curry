{- 
  
  Euler #40 in Curry

  """
  An irrational decimal fraction is created by concatenating the positive integers:
   
  0.123456789101112131415161718192021...
   
  It can be seen that the 12th digit of the fractional part is 1.

  If dn represents the nth digit of the fractional part, find the 
  value of the following expression.
  
  d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Data.Char
import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
-- import CLP.FD


euler40a :: Int
euler40a =  product [digitToInt $ ds !! ((10^i)-1)  | i <- [0..5]]
            where
            ds = concat [show n | n <- [1..210000]]

-- Non-det.
-- Much slower than euler40a
euler40b' :: Int
euler40b' = digitToInt $ ds !! ((anyOf [1,10,100,1000,10000,100000,1000000]) -1)
            where
            ds = concat [show n | n <- [1..210000]]

euler40b :: Int
euler40b = product $ allValues euler40b'

-- Recursive version
euler40c' :: [Char] -> Int -> Int -> Int
euler40c' ds i aux
      | i == 5    = aux * (d i)
      | otherwise = euler40c' ds (i+1) ((d i) * aux)
      where
      d j = digitToInt $ ds !! ((10^j)-1)

euler40c :: Int
euler40c =  euler40c' (concat [show n | n <- [1..210000]]) 0 1


main :: IO ()
main = do
         -- PAKCS: Execution time: 784 msec. / elapsed: 1031 msec.
         -- KICS2: GHC compilation time: 1.10s / elapsed: 0:01.46 Execution time: 0.03s / elapsed: 0:00.04
         -- Curry2Go: Compilation time: 1.95s / elapsed: 0:01.53 Execution time: 3.88s / elapsed: 0:01.64
         print euler40a

         -- PAKCS: Execution time: 7542 msec. / elapsed: 10045 msec.
         -- KICS2: KiCS2 compilation time: 1.79s / elapsed: 0:02.14 GHC compilation time: 1.40s / elapsed: 0:01.80 Execution time: 22.83s / elapsed: 0:24.94
         -- Curry2Go: Compilation time: 2.06s / elapsed: 0:01.69 Execution time: 82.84s / elapsed: 0:18.40
         -- print euler40b


         -- PAKCS: Execution time: 787 msec. / elapsed: 1042 msec.
         -- KICS2: KiCS2 compilation time: 1.72s / elapsed: 0:02.11 GHC compilation time: 1.43s / elapsed: 0:01.87 Execution time: 0.02s / elapsed: 0:00.03
         -- Euler2Go: Compilation time: 2.08s / elapsed: 0:01.45 Execution time: 3.65s / elapsed: 0:01.58
         -- print euler40c