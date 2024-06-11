{- 
  
  N-queens problem in Curry


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils
-- import CLP.FD
import Debug.Trace



nqueens n = [xs | xs <- permutations [1..n],
                  allDifferent3 [ (xs !! i) - i | i <- [0..n-1]],
                  allDifferent3 [ (xs !! i) + i | i <- [0..n-1]]
            ]


main1 = nqueens 4

--
-- PAKCS: Execution time: 5042 msec. / elapsed: 5717 msec.
-- KICS2: KiCS2 compilation time: 1.98s / elapsed: 0:02.49 GHC compilation time: 1.33s / elapsed: 0:01.73 Execution time: 0.11s / elapsed: 0:00.12
-- Curry2Go: Compilation time: 1.84s / elapsed: 0:01.41 Execution time: 15.20s / elapsed: 0:09.66
--
main2 = length $ nqueens 8


--
-- PAKCS: Execution time: 690639 msec. / elapsed: 801527 msec.
-- KICS2: KiCS2 compilation time: 2.03s / elapsed: 0:02.47 GHC compilation time: 1.26s / elapsed: 0:01.71 Execution time: 11.06s / elapsed: 0:11.07
-- Curry2Go:
--
main3 = head $ nqueens 12


main = do
          print main3 