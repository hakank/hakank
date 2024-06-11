{- 
  
  Place number puzzle in Curry CLP.FD

  http://ai.uwaterloo.ca/~vanbeek/Courses/Slides/introduction.pdf
  """
  Place numbers 1 through 8 on nodes
  - each number appears exactly once
  - no connected nodes have consecutive numbers
       2 - 5 
     / | X | \
   1 - 3 - 6 - 8
     \ | X | /
       4 - 7
  """

  NOPE. The decompositions of abs does not work here (or is very very slow).


  This is a port of my Picat model http://hakank.org/picat/place_number_puzzle.pi

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtilsCLPFD (abs2,elementVal)
import CLP.FD
import Debug.Trace


-- 1 based
-- graph1 = [[1,2], [1,3], [1,4],
--           [2,1], [2,3], [2,5],
--           [3,1], [3,2], [3,4], [3,5],[3,6],[3,7],
--           [4,1], [4,3], [4,6], [4,7],
--           [5,2], [5,3], [5,6], [5,8],
--           [6,2], [6,3], [6,4], [6,5],[6,7],[6,8],
--           [7,3], [7,4], [7,6], [7,8],
--           [8,5], [8,6], [8,7]]

-- 0-based
graph = [[0,1],[0,2],[0,3],
         [1,0],[1,2],[1,4],
         [2,0],[2,1],[2,3],[2,4],[2,5],[2,6],
         [3,0],[3,2],[3,5],[3,6],
         [4,1],[4,2],[4,5],[4,7],
         [5,1],[5,2],[5,3],[5,4],[5,6],[5,7],
         [6,2],[6,3],[6,5],[6,7],
         [7,4],[7,5],[7,6]]

check gss xs 
  | gss == [] = true
  | otherwise = (abs2 (xs !! g1) (xs !! g2) v /\ v ># 1) /\ check gs xs
                where
                  ([g1,g2]:gs) = gss
                  v = head (domain 2 ((length xs)-1))

check2 gss dss xs 
  | gss == [] = true
  | otherwise = (abs2 (xs !! g1) (xs !! g2) d) /\ check2 gs ds xs
                where
                  ([g1,g2]:gs) = gss
                  (d:ds) = dss

-- diff x1 x2 = ((x1 - x2) ># 1) ? ((x1 - x2) <# (-1))
diff x1 x2 = (x1 - x2) ># 1
diff x1 x2 = (x1 - x2) <# (-1)

place_number_puzzle g = let
                          n = 8
                          n1 = n-1
                          xs = take n (domain 1 n)
                          ds = take (length g) (domain 2 n)
                        in
                          solveFDAll [FirstFailConstrained,Bisect] xs $
                          allDifferent xs /\
                          -- check g xs
                          -- check2 g ds xs
                          -- foldl1 (/\) [ diff (xs !! g1) (xs !! g2)
                          --              | [g1,g2] <- g]
                          -- foldl1 (/\) [ abs2 (xs !! g1) (xs !! g2) v 
                          --              | [g1,g2] <- g,
                          --                let v = head (domain 2 n1) -- v must be > 1
                          --             ]
                          foldl1 (/\) [ abs2 (xs !! g1) (xs !! g2) d
                                       | ([g1,g2],d) <- zip g ds
                                      ]

main = mapM_ print $ place_number_puzzle graph


-- non cp variant
place_number_puzzle2 g = sort [xs | xs <- permutations [1..8],
                              and [abs ((xs !! g1) - (xs !! g2)) > 1 | [g1,g2] <- g]                              
                              ]

{-

  There are two distinct solutions, the last two are the reverse of the first two.

[2,5,8,6,3,1,4,7]
[2,6,8,5,4,1,3,7]
[7,3,1,4,5,8,6,2]
[7,4,1,3,6,8,5,2]
Execution time: 3413 msec. / elapsed: 3966 msec.
-}
main2 = mapM_ print $ place_number_puzzle2 graph


