{- 
  
  N-queens problem in Haskell


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Hakell page: http://hakank.org/haskell/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils


nqueens n = [xs | xs <- permutations [1..n],
                  allDifferent3 [ (xs !! i) - i | i <- [0..n-1]],
                  allDifferent3 [ (xs !! i) + i | i <- [0..n-1]]
            ]


main1 = print $ nqueens 4

main2 = length $ nqueens 8 -- (0.15 secs, 121,239,592 bytes)

main3 = head $ nqueens 12 -- (13.32 secs, 14,895,152,368 bytes) compiled -O:  1.681s

main = do
         print main3
