{- 
  
  Euler #45 in Curry

  """  
  Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:

  Triangle 	  	Tn=n(n+1)/2 	  	1, 3, 6, 10, 15, ...
  Pentagonal 	  	Pn=n(3n−1)/2 	  	1, 5, 12, 22, 35, ...
  Hexagonal 	  	Hn=n(2n−1) 	  	1, 6, 15, 28, 45, ...

  It can be verified that T(285) = P(165) = H(143) = 40755.

  Find the next triangle number that is also pentagonal and hexagonal.
  """


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
-- import CLP.FD

pent :: Int -> Int
pent(n) = n*(3*n-1) `div` 2

tri :: Int -> Int
tri(n)  = n*(n+1) `div` 2

hex :: Int -> Int
hex(n)  = n*(2*n-1)

euler45a' :: Int -> Int -> Int -> Int
euler45a' p t h
   | pp == tt && tt == hh = pp
   | otherwise = euler45a' p2 t2 h2
   where
      pp = pent p
      tt = tri t
      hh = hex h
      -- next step
      t2 = t+1
      tt2 = tri t2
      p2 = if tt2 > pp then p+1 else p
      h2 = if pp > hh || tt2 > hh then h+1 else h
      

euler45a :: Int
euler45a = euler45a' 166 286 144

main :: IO ()
main = do
          -- PAKCS: Execution time: 1327 msec. / elapsed: 2145 msec.
          -- KICS2: GHC compilation time: 1.18s / elapsed: 0:01.52 Execution time: 0.08s / elapsed: 0:00.10
          -- Curry2Go: Compilation time: 2.14s / elapsed: 0:01.50 Execution time: 9.44s / elapsed: 0:03.87
          print euler45a