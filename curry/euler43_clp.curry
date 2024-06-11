{- 
  
  Euler #43 in Curry

  """  
  The number, 1406357289, is a 0 to 9 pandigital number because it is made up of 
  each of the digits 0 to 9 in some order, but it also has a rather interesting 
  sub-string divisibility property.
  
  Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we 
  note the following:
  
      * d2d3d4=406 is divisible by 2
      * d3d4d5=063 is divisible by 3
      * d4d5d6=635 is divisible by 5
      * d5d6d7=357 is divisible by 7
      * d6d7d8=572 is divisible by 11
      * d7d8d9=728 is divisible by 13
      * d8d9d10=289 is divisible by 17
  
  Find the sum of all 0 to 9 pandigital numbers with this property.
  """

  This is a CLP.FD version (PAKCS only).

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (digitsToNum,runningChunksOf)
import HakankUtilsCLPFD (mod2)
import CLP.FD

--
-- First approach, very explicit
--
euler43_clp_a' :: [[Int]]
euler43_clp_a' =  let
                     [a,b,c,d,e,f,g,h,i,j] = take 10 (domain 0 9)
                  in
                     solveFDAll [FirstFailConstrained,Bisect] [a,b,c,d,e,f,g,h,i,j]  $
                     allDifferent [a,b,c,d,e,f,g,h,i,j]  /\                   
                     mod2 (b*100 + c*10 + d)  2 0 999 /\
                     mod2 (c*100 + d*10 + e)  3 0 999 /\
                     mod2 (d*100 + e*10 + f)  5 0 999 /\
                     mod2 (e*100 + f*10 + g)  7 0 999 /\
                     mod2 (f*100 + g*10 + h) 11 0 999 /\
                     mod2 (g*100 + h*10 + i) 13 0 999 /\
                     mod2 (h*100 + i*10 + j) 17 0 999

euler43_clp_a :: Int
euler43_clp_a = Data.List.sum $ map digitsToNum $ euler43_clp_a'


--
-- Another version, a little more general using only the list x.
--
euler43_clp_b' :: [[Int]]
euler43_clp_b' =  let
                     x = take 10 (domain 0 9)
                  in
                     solveFDAll [FirstFailConstrained,Bisect] x $
                     allDifferent x /\
                     constraints_b x

dd :: [CLP.FD.FDExpr] -> Int -> Int -> Int -> CLP.FD.FDExpr -> CLP.FD.FDConstr
dd x p q r m  = mod2 (100*(x !! p) + 10*(x !! q) + (x !! r)) m 0 999

constraints_b :: [CLP.FD.FDExpr] -> CLP.FD.FDConstr
constraints_b x =  dd x 1 2 3  2 /\
                   dd x 2 3 4  3 /\
                   dd x 3 4 5  5 /\
                   dd x 4 5 6  7 /\
                   dd x 5 6 7 11 /\
                   dd x 6 7 8 13 /\
                   dd x 7 8 9 17 

euler43_clp_b :: Int
euler43_clp_b = Data.List.sum $ map digitsToNum $ euler43_clp_b'


--
-- A neater version.
--
euler43_clp_c' :: [[Int]]
euler43_clp_c' =  let
                    x = take 10 (domain 0 9)
                  in
                    solveFDAll [FirstFailConstrained,Bisect] x $
                    allDifferent x /\
                    constraints_c x 1 [2,3,5,7,11,13,17]

constraints_c :: [CLP.FD.FDExpr] -> Int -> [CLP.FD.FDExpr] -> CLP.FD.FDConstr
constraints_c _ _ [] = true
constraints_c x i (p:ps) = mod2 (100*(x !! i) + 10*(x !! (i+1)) + (x !! (i+2))) p 0 999 /\ constraints_c x (i+1) ps


euler43_clp_c :: Int
euler43_clp_c = Data.List.sum $ map digitsToNum $ euler43_clp_c'


main :: IO ()
main = do
         -- PAKCS: Execution time: 19 msec. / elapsed: 24 msec.
         -- KICS2: no support for CLP.FD
         -- Curry2Go: no support for CLP.FD
         -- print euler43_clp_a


         -- PAKCS:Execution time: 25 msec. / elapsed: 25 msec.
         -- KICS2: no support for CLP.FD
         -- Curry2Go: no support for CLP.FD
         -- print euler43_clp_b


         -- PAKCS: Execution time: 16 msec. / elapsed: 16 msec.
         -- KICS2: no support for CLP.FD
         -- Curry2Go: no support for CLP.FD
         print euler43_clp_c