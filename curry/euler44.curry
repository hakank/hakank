{- 

  Euler #44 in Curry

  """  
  Pentagonal numbers are generated by the formula, P(n)=n(3n−1)/2. 
  The first ten pentagonal numbers are:

  1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...

  It can be seen that P(4) + P(7) = 22 + 70 = 92 = P(8). However, 
  their difference,  70 − 22 = 48, is not pentagonal.

  Find the pair of pentagonal numbers, P(j) and P(k), for which their sum 
  and difference is pentagonal and D = |P(k) − P(j)| is minimised; what 
  is the value of D?  
  """

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Curry page: http://www.hakank.org/curry/

-}


import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
-- import CLP.FD
import Debug.Trace

pent :: Int -> Int
pent n = n*((3*n)-1) `div` 2

isPent :: Int -> Bool
isPent x = if not $ null t then True else False
           where
              t =  [ n | n <- [1..floor$ sqrt(fromIntegral x)], pent n == x]

euler44a :: Int
euler44a = minimum [p2-p1 | p1 <- ps, p2 <- ps, p1 <= p2, elem (p2-p1) ps, elem (p1 + p2) ps]
           where
           ps = reverse $ map pent [1..2500]

-- Somewhat cheating
euler44b :: Int
euler44b = head [p2-p1 | p1 <- ps, p2 <- ps, p1 <= p2, elem (p2-p1) ps, elem (p1 + p2) ps]
           where
           ps = reverse $ map pent [1..2500]

-- Infinite list: Memory exhaused
euler44c :: Int
euler44c = head [p2-p1 | p1 <- ps, p2 <- ps, p1 <= p2,  elem (p2-p1) ps, elem (p1 + p2) ps]
           where
           ps = map pent [1..]


-- Using Data.Set
euler44d :: Int
euler44d = head [p2-p1 | p1 <- ps, p2 <- ps, p1 <= p2, Set.member (p2-p1) pss, Set.member (p1 + p2) pss]
           where
           ps = map pent [1..2500]
           pss = Set.fromList ps



main :: IO ()
main = do
         -- PAKCS: -
         -- KICS2: KiCS2 compilation time: 1.63s / elapsed: 0:01.83 GHC compilation time: 2.05s / elapsed: 0:02.54 Command terminated by signal 2 Execution time: 179.96s / elapsed: 3:01.52
         -- Curry2Go: -
         -- print euler44a

         -- PAKCS: > 30min
         -- KICS2: KiCS2 compilation time: 1.13s / elapsed: 0:01.35 GHC compilation time: 2.14s / elapsed: 0:02.51 Execution time: 97.99s / elapsed: 1:38.10
         -- Curry2Go: >30min
         -- print euler44b

         -- PAKCS: -
         -- KICS2: Out of memory
         -- Curry2Go: -         
         -- print euler44c


         -- PAKCS: Execution time: 126913 msec. / elapsed: 144845 msec.
         -- KICS2: KiCS2 compilation time: 1.23s / elapsed: 0:01.45 GHC compilation time: 1.35s / elapsed: 0:01.76 Execution time: 1.83s / elapsed: 0:01.85
         -- Curry2Go: Compilation time: 2.28s / elapsed: 0:01.53 Execution time: 619.57s / elapsed: 4:46.68
         print euler44d