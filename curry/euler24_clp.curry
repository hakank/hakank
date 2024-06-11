{- 
  
  Euler #24 CLP in Curry

  """
  A permutation is an ordered arrangement of objects. For example, 3124 is one 
  possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are 
  listed numerically or alphabetically, we call it lexicographic order. The 
  lexicographic permutations of 0, 1 and 2 are:
   
      012   021   102   120   201   210
  
  What is the millionth lexicographic permutation of the digits 
  0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
  """ 

  This is a CLP version in a separate file since only PAKCS supports CLP.FD

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Control.AllValues
import Control.SetFunctions
-- import HakankUtils
import CLP.FD


allDiff :: Int -> [Int]
allDiff n = let
              a = take n (domain 0 9)
            in
              solveFD [] a $
              -- slower with this constraint since then we have to sort the solutions
              -- (head a) ># 0 /\  
              allDifferent a

-- allValues
euler24_clp_a :: [Int]
euler24_clp_a = (allValues (allDiff 10)) !! (1000000-1)

euler24_clp_b :: [Int]
euler24_clp_b = last $ take 1000000 $ allValues $ allDiff 10

euler24_clp_c :: [Int]
euler24_clp_c = head $ drop (1000000-1) $ allValues $ allDiff 10

-- values2list $ set1
euler24_clp_d :: IO [Int]
euler24_clp_d = fmap (last . take 1000000) $ values2list $ set1 allDiff 10



main :: [Int]
main = do
         -- PAKCS: Execution time: 25723 msec. / elapsed: 28191 msec.
         -- euler24_clp_a

         -- PAKCS: Execution time: 23392 msec. / elapsed: 25815 msec.
         euler24_clp_b

         -- PAKCS: Execution time: 23354 msec. / elapsed: 25298 msec.
         -- euler24_clp_c

         -- PAKCS: Execution time: 23953 msec. / elapsed: 26564 msec
         -- euler24_clp_d