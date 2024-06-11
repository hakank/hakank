{- 
  
  Euler #31 in Curry

  Problem 31
  """
  In England the currency is made up of pound, £, and pence, p, and 
  there are eight coins in general circulation:

     1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

  It is possible to make £2 in the following way:

     1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

  How many different ways can £2 be made using any number of coins?
  """

  This is a CLP.FD version. Only supported by PAKCS.

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Control.AllValues
-- import Control.SetFunctions
import HakankUtils
import CLP.FD


euler31_clpa' = let
                  [a,b,c,d,e,f,g,h] = take 8 (domain 0 200)
               in
                  solveFD [] [a,b,c,d,e,f,g,h] $
                  (1*a) + (2*b) + (5*c) + (10*d) + (20*e) + (50*f) + (100*g) + (200*h) =# 200

euler31_clpa = length . allValues $ euler31_clpa'


--
-- Smaller explicit domains: not faster (so CLP.FD reduces it automatically)
--
euler31_clpb' = let
                  a = head (domain 0 200)
                  b = head (domain 0 (200 `div`   2))
                  c = head (domain 0 (200 `div`   5))
                  d = head (domain 0 (200 `div`  10))
                  e = head (domain 0 (200 `div`  20))
                  f = head (domain 0 (200 `div`  50))
                  g = head (domain 0 (200 `div` 100))
                  h = head (domain 0 (200 `div` 200))                 
               in
                  solveFD [] [a,b,c,d,e,f,g,h] $
                  (1*a) + (2*b) + (5*c) + (10*d) + (20*e) + (50*f) + (100*g) + (200*h) =# 200

euler31_clpb = length . allValues $ euler31_clpb'


--
-- Neater model using scalarProduct: not faster
--
euler31_clpc' = let
                  [a,b,c,d,e,f,g,h] = take 8 (domain 0 200)
               in
                  solveFD [] [a,b,c,d,e,f,g,h] $
                  scalarProduct weights [a,b,c,d,e,f,g,h] Equ 200
               where
                  weights = [1,2,5,10,20,50,100,200]

euler31_clpc = length . allValues $ euler31_clpc'



main = do
         -- PAKCS: Execution time: 2151 msec. / elapsed: 2193 msec.
         -- KICS2: Does not support CLP.FD
         -- Curry2Go: Does not support CLP.FD         
         euler31_clpa


         -- PAKCS: Execution time: 2160 msec. / elapsed: 2234 msec.
         -- KICS2: Does not support CLP.FD
         -- Curry2Go: Does not support CLP.FD         
         -- euler31_clpb


         -- PAKCS: Execution time: 2197 msec. / elapsed: 2256 msec
         -- KICS2: Does not support CLP.FD
         -- Curry2Go: Does not support CLP.FD         
         -- euler31_clpc
