{- 
  
  Euler #24 in Haskell

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
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List

euler24a = (sort $ permutations [0..9]) !! (1000000-1)

main = do
         print euler24a -- (6.17 secs, 3,134,646,432 bytes)
