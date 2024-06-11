{- 
  
  Euler #39 in Curry

  """
  If p is the perimeter of a right angle triangle with integral length sides, 
  {a,b,c}, there are exactly three solutions for p = 120.
   
  {20,48,52}, {24,45,51}, {30,40,50}
   
  For which value of p <= 1000, is the number of solutions maximised?
  """

  Also see euler39_clp.curry for a CLP.FD version (PAKCS only).

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (collect, collect2)
-- import CLP.FD

-- Brute force
euler39a :: (Int,Int)
euler39a = maximum $ map (\(n,c) -> (c,n)) $ collect $ sort [t | a <- [1..500], b <- [a..500], c <- [b..500], a^2 + b^2 == c^2, let t = a+b+c, t <= 1000]


euler39b :: (Int,Int)
euler39b =  maximum $ map (\(n,c) -> (c,n)) $ collect $ sort [t | a <- squares, b <- squares, a < b, elem (a + b) squares, let t = sq a b, t <= 1000]
         where squares = [n^2 | n <- [1..500]]
               sq a b = floor $ (sqrt a2) + (sqrt b2) + (sqrt (a2+b2))
                    where a2 = fromIntegral a
                          b2 = fromIntegral b

--
-- collect2 instead of map (...) collect.
-- Not faster than euler39b
euler39c :: (Int,Int)
euler39c =  maximum $ collect2 $ sort [t | a <- squares, b <- squares, a < b, elem (a + b) squares, let t = sq a b, t <= 1000]
         where squares = [n^2 | n <- [1..500]]
               sq a b = floor $ (sqrt a2) + (sqrt b2) + (sqrt (a2+b2))
                    where a2 = fromIntegral a
                          b2 = fromIntegral b


--
-- euler39b but using maximumBy instead of maximum. Not faster.
--
euler39d :: (Int,Int)
euler39d =  maximumBy (\a b -> (snd a) `compare` (snd b)) $ collect $ sort [t | a <- squares, b <- squares, a < b, elem (a + b) squares, let t = sq a b, t <= 1000]
         where squares = [n^2 | n <- [1..500]]
               sq a b = floor $ (sqrt a2) + (sqrt b2) + (sqrt (a2+b2))
                    where a2 = fromIntegral a
                          b2 = fromIntegral b

main :: IO ()
main = do
         -- PAKCS: euler39 Data.List> euler39a Execution time: 2529235 msec. / elapsed: 2929516 msec.
         -- KICS2: GHC compilation time: 1.62s / elapsed: 0:02.07 Execution time: 37.11s / elapsed: 0:37.28
         -- Curry2Go: Compilation time: 2.18s / elapsed: 0:01.69 Execution time: 13202.01s / elapsed: 1:33:41
         -- print euler39a
         
         -- PAKCS: Execution time: 90167 msec. / elapsed: 100299 msec.
         -- KICS2: KiCS2 compilation time: 2.26s / elapsed: 0:02.62 GHC compilation time: 1.57s / elapsed: 0:02.02 Execution time: 1.94s / elapsed: 0:01.95
         -- Curry2Go: Compilation time: 2.13s / elapsed: 0:01.70 Execution time: 276.59s / elapsed: 2:50.50
         print euler39b

         -- PAKCS: Execution time: 90436 msec. / elapsed: 100650 msec.
         -- KICS2: KiCS2 compilation time: 1.64s / elapsed: 0:02.03 GHC compilation time: 1.31s / elapsed: 0:01.73 Execution time: 1.96s / elapsed: 0:01.97
         -- Curry2Go: Compilation time: 2.31s / elapsed: 0:01.60 Execution time: 275.06s / elapsed: 2:49.51
         -- print euler39c


         -- PAKCS: -
         -- KICS2: KiCS2 compilation time: 2.39s / elapsed: 0:02.79 GHC compilation time: 1.59s / elapsed: 0:02.07 Execution time: 1.95s / elapsed: 0:01.97 
         -- Curry2Go: -
         -- print euler39b