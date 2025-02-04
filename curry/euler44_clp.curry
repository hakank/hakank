{- 

  Euler #44 in Curry CLP.FD

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

  This is a CLP.FD version of euler44.curry

  Nope, too slow...

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Curry page: http://www.hakank.org/curry/

-}


import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import CLP.FD

pent :: Int -> Int
pent n = n*((3*n)-1) `div` 2

isPent :: Int -> Bool
isPent x = if not $ null t then True else False
           where
              t =  [ n | n <- [1..floor$ sqrt(fromIntegral x)], pent n == x]

euler44_clp = let  
                 [n1,n2,n3,n4] = take 4 (domain 1 99999) -- the integers
                 [p1,p2,p3,p4] = take 4 (domain 1 99999) -- the pentagonal numbers
              in
                 solveFD [FirstFail,Bisect] [p1,p2] $
                 n1 <=# n2             /\
                 p1*2 =# n1*((3*n1)-1) /\ -- enforce that p1..p4 are pentagonal numbers
                 p2*2 =# n2*((3*n2)-1) /\
                 p3*2 =# n3*((3*n3)-1) /\
                 p4*2 =# n4*((3*n4)-1) /\
                 p3 =# p2-p1           /\ -- p2-p1 and p1+p2 are both pentagonal numbers
                 p4 =# p1+p2
                 

main :: IO ()
main = do
         -- PAKCS:
         print euler44_clp

