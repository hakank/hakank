{- 
  
  Ackermann function in Curry

  https://en.wikipedia.org/wiki/Ackermann_function


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
-- import CLP.FD

-- a :: Int -> Int -> Int -> Int
-- This is non-det
a0 0 n = n + 1
a0 m 0 = a0 (m-1) 1
a0 m n | m > 0 && n > 0 = a0 (m-1) (a0 m (n-1))

-- [(0,0,(Just 1)),(0,1,(Just 2)),(0,2,(Just 3)),(1,0,(Just 2)),(1,1,(Just 3)),(1,2,(Just 4)),(2,0,(Just 3)),(2,1,(Just 5)),(2,2,(Just 7))]
-- a0 3 1 -> out of memory (in KiCS2)
--
test0 = [(m,n,oneValue $ a0 m n) | m <- [0..2], n <- [0..2]]

-- deterministic
a m n
   | m == 0 = n + 1
   | n == 0 = a (m-1) 1
   | otherwise = a (m-1) (a m (n-1))


-- PAKCS: Execution time: 354332 msec. / elapsed: 423717 msec.
-- KICS2: KiCS2 compilation time: 1.50s / elapsed: 0:01.84 GHC compilation time: 1.27s / elapsed: 0:01.62 Execution time: 6.02s / elapsed: 0:06.04
-- Curry2Go: ompilation time: 2.18s / elapsed: 0:02.02 Execution time: 1902.86s / elapsed: 13:39.90
test1 = [(m,n,a m n) | m <- [0..3], n <- [0..10]]

main :: IO ()
main = do
         print test1