{- 
  
  Photo problem in Haskell

  Problem statement from Mozart/Oz tutorial:
  http://www.mozart-oz.org/home/doc/fdt/node37.html#section.reified.photo
  """
  Betty, Chris, Donald, Fred, Gary, Mary, and Paul want to align in one row for 
  taking a photo. Some of them have preferences next to whom they want to stand:
 
     1. Betty wants to stand next to Gary and Mary.
     2. Chris wants to stand next to Betty and Gary.
     3. Fred wants to stand next to Mary and Donald.
     4. Paul wants to stand next to Fred and Donald.
 
  Obviously, it is impossible to satisfy all preferences. Can you find an alignment 
  that maximizes the number of satisfied preferences?
  """

  Oz solution: 
    6 # alignment(betty:5  chris:6  donald:1  fred:3  gary:7   mary:4   paul:2)
  [5, 6, 1, 3, 7, 4, 2]
  
   
  There are 8 solutions:
 
  positions = [3, 1, 6, 5, 2, 4, 7]
  positions = [3, 1, 7, 5, 2, 4, 6]
  positions = [3, 2, 6, 5, 1, 4, 7]
  positions = [3, 2, 7, 5, 1, 4, 6]
  positions = [5, 6, 1, 3, 7, 4, 2]  (the Oz solution.)
  positions = [5, 6, 2, 3, 7, 4, 1]
  positions = [5, 7, 1, 3, 6, 4, 2]
  positions = [5, 7, 2, 3, 6, 4, 1]

  This is a port of my Curry program photo_problem.curry

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List

--
-- List comprehension
--
photo_problem2 (n,p) = mapM_ print $ allOptSols
                       where 
                         sols = sort [ (s,positions) | positions <- permutations [1..n],
                                       let s = Data.List.sum [abs ((positions !! (p1-1)) - (positions !! (p2-1))) |
                                                          [p1,p2] <- p]  ]
                         (opt,_) = head sols
                         allOptSols = [ sol | (obj,sol) <- sols, obj == opt]



{-
  [3,1,6,5,2,4,7]
  [3,1,7,5,2,4,6]
  [3,2,6,5,1,4,7]
  [3,2,7,5,1,4,6]
  [5,6,1,3,7,4,2]
  [5,6,2,3,7,4,1]
  [5,7,1,3,6,4,2]
  [5,7,2,3,6,4,1]
  (0.11 secs, 29,809,992 bytes)

-}
main2 :: IO ()
main2 = photo_problem2 preferences1

{-
  [4,8,3,5,6,7,1,2,9,10,11]
  [4,8,3,6,5,7,1,2,9,10,11]
  [4,9,3,5,6,7,1,2,8,10,11]
  [4,9,3,6,5,7,1,2,8,10,11]
  [5,8,3,4,6,7,1,2,9,10,11]
  [5,9,3,4,6,7,1,2,8,10,11]
  [7,3,9,8,6,5,11,10,4,2,1]
  [7,4,9,8,6,5,11,10,3,2,1]
  [8,3,9,6,7,5,11,10,4,2,1]
  [8,3,9,7,6,5,11,10,4,2,1]
  [8,4,9,6,7,5,11,10,3,2,1]
  [8,4,9,7,6,5,11,10,3,2,1]
  (877.33 secs, 504,384,182,760 bytes)

  Compiled (-O): Prints all the solutions but ends after 171.74s with Out of memory
  Compiled (no -O): 

-}
main3 :: IO ()
main3 = photo_problem2 preferences2

{-
  [5,1,7,6,4,3,9,8,2]
  [5,9,3,4,6,7,1,2,8]
  (6.36 secs, 4,007,289,240 bytes)

-}
main4 :: IO ()
main4 = photo_problem2 preferences3


main = main3
        
--
-- Problem instances.
--
-- Note that data is 1 based and is converted to 0 based in the
-- photo_problem function.
--


--
-- Problem 1 (see above):
-- 1. Betty wants to stand next to Gary and Mary.
--     1 : 5, 6
-- 2. Chris wants to stand next to Betty and Gary.
--     2 : 1, 5
-- 3. Fred wants to stand next to Mary and Donald.
--     4 : 6, 3
-- 4. Paul wants to stand next to Fred and Donald.
--     7 : 4, 3
--
-- preferences(ProblemNumber, NumberOfPersons, Preferences)
preferences1 :: (Int,[[Int]])
preferences1 = (7,[[1,5],
                   [1,6],
                   [2,1],
                   [2,5],
                   [4,6],
                   [4,3],
                   [7,4],
                   [7,3]])


               
-- From http://www.g12.cs.mu.oz.au/minizinc/photo.data2
preferences2 :: (Int,[[Int]])
preferences2 = (11,[[1,3], 
                    [1,5], 
                    [1,8], 
                    [2,5], 
                    [2,9], 
                    [3,4], 
                    [3,5], 
                    [4,1], 
                    [4,5], 
                    [4,10],
                    [5,6], 
                    [5,1], 
                    [6,1], 
                    [6,9], 
                    [7,3],
                    [7,8], 
                    [8,9],
                    [8,7], 
                    [9,10], 
                    [10,11]])


-- From http://www.ampl.com/NEW/LOGIC/EXAMPLES/photo9.dat
-- (This seems to be a simplified of #2
preferences3 :: (Int,[[Int]])
preferences3 = (9,[[1,3], 
                   [1,5], 
                   [1,8], 
                   [2,5], 
                   [2,9], 
                   [3,4], 
                   [3,5], 
                   [4,1], 
                   [4,5], 
                   [5,1], 
                   [5,6], 
                   [6,1], 
                   [6,9], 
                   [7,3],
                   [7,8], 
                   [8,7],
                   [8,9]])
