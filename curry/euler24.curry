{- 
  
  Euler #24 in Curry

  """
  A permutation is an ordered arrangement of objects. For example, 3124 is one 
  possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are 
  listed numerically or alphabetically, we call it lexicographic order. The 
  lexicographic permutations of 0, 1 and 2 are:
   
      012   021   102   120   201   210
  
  What is the millionth lexicographic permutation of the digits 
  0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
  """ 

  For a CLP.FD version (PAKCS), see euler24_clp.curry which is quite faster than
  PAKCS' plain Curry version:
    PAKCS CLP  : Execution time: 25723 msec. / elapsed: 28191 msec.
    PAKCS plain: Execution time: 322912 msec. / elapsed: 520755 msec.

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
-- import CLP.FD

euler24a :: [Int]
euler24a = (sort $ permutations [0..9]) !! (1000000-1)

main :: IO ()
main = do
         -- PAKCS: Execution time: 322912 msec. / elapsed: 520755 msec.
         -- Cf  euler24a_clp.curry: Execution time: 25723 msec. / elapsed: 28191 msec.
         -- KICS2: KiCS2 compilation time: 1.10s / elapsed: 0:01.31 GHC compilation time: 1.11s / elapsed: 0:01.46 Execution time: 9.48s / elapsed: 0:10.26
         -- Curry2Go: Compilation time: 1.91s / elapsed: 0:02.14 Execution time: 2186.09s / elapsed: 10:03.34
         print euler24a
