{- 
  
  Euler #39 in Curry CLP.FD

  """
  If p is the perimeter of a right angle triangle with integral length sides, 
  {a,b,c}, there are exactly three solutions for p = 120.
   
  {20,48,52}, {24,45,51}, {30,40,50}
   
  For which value of p <= 1000, is the number of solutions maximised?
  """

  This is a CLP.FD version (for PAKCS).

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (collect, collect2)
-- import HakankUtilsCLPFD
import CLP.FD

p39 :: Int -> [[Int]]
p39 i = let
            [a,b,c] = take 3 (domain 1 (1 + i `div` 2))
            ii = head (domain i i)
         in
            solveFDAll [FirstFailConstrained,Bisect] [a,b,c] $
            a <=# b /\
            b <=# c /\
            ii =# a + b + c /\  -- note: i #= a+b+c  is not allowed
            a*a + b*b =# c*c

euler39_clp :: (Int,Int)
euler39_clp = maximum [(length $ p39 n,n) | n <- [1..1000]]

main :: IO ()
main = do
         -- PAKCS: Execution time: 1743 msec. / elapsed: 1754 msec.
         print euler39_clp