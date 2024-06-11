{- 
  
  Photo problem in Curry CLP.FD

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

  This is a port of my Picat model http://hakank.org/picat/photo_problem.pi

  For a list comprehension version, see photo_problem_list_comprehension.curry

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import HakankUtilsCLPFD (abs2)
import CLP.FD
import Debug.Trace

photoProblem :: (Int,[[Int]]) -> [[Int]]
photoProblem (n,p) = let
                        numPrefs = length p
                        positions = take n (domain 1 n)
                        -- The differences (to be minimized)
                        z = head (domain 0 (n*n))
                        diffs = take numPrefs (domain 1 n)
                      in
                        solveFDAll [FirstFail] (z:positions) $
                        z =# Data.List.sum diffs /\
                        allDifferent positions /\
                        foldl1 (/\) [abs2 (positions !! (p1-1)) (positions !! (p2-1)) (diffs !! i)
                                     | ([p1,p2],i) <- zip p [0..numPrefs-1] ]


-- Alternative optimization by using  Minimize zOpt (non-det) and solveFDOne
-- Nope, this is much slower than photoProblem ...
--
photoProblem2 :: (Int, [[Int]]) -> [Int]
photoProblem2 (n,p) = let
                        numPrefs = length p
                        positions = take n (domain 1 n)
                        -- The differences (to be minimized)
                        z = head (domain 0 (n*n))
                        zOpt = anyOf [0..(n*n)]
                        diffs = take numPrefs (domain 1 n)
                      in
                        solveFDOne [FirstFailConstrained,Bisect,Minimize zOpt] (z:positions) $
                        z =# Data.List.sum diffs /\
                        traceShow (show zOpt) fd zOpt =# z /\ -- tracing the progression of zOpt
                        allDifferent positions /\
                        foldl1 (/\) [abs2 (positions !! (p1-1)) (positions !! (p2-1)) (diffs !! i)
                                     | ([p1,p2],i) <- zip p [0..numPrefs-1] ]


printSol :: (Int, [[Int]]) -> IO ()
printSol p = mapM_ print $ allOptSols
       where
         sols = sort $ photoProblem p
         (opt:_) = head sols
         allOptSols = [tail sol | sol <- sols, head sol == opt]

{-
  [3,1,6,5,2,4,7]
  [3,1,7,5,2,4,6]
  [3,2,6,5,1,4,7]
  [3,2,7,5,1,4,6]
  [5,6,1,3,7,4,2]
  [5,6,2,3,7,4,1]
  [5,7,1,3,6,4,2]
  [5,7,2,3,6,4,1]
  Execution time: 1970 msec. / elapsed: 2045 msec.

-}
main :: IO ()
main = printSol preferences1


{-
  Testing of photoProblem2: much slower to even get one optimal solution.
  Also tracing the progression of zOpt
  "0"
  "1"
  "2"
  "3"
  "4"
  "5"
  "6"
  "7"
  "8"
  "9"
  "10"
  [10,3,2,6,5,1,4,7]
  Execution time: 12098 msec. / elapsed: 12097 msec.

-}
main1 :: [Int]
main1 =  photoProblem2 preferences1



{-
  Too slow  
-}
main2 :: IO ()
main2 = printSol preferences2

{-
  Too slow
-}
main3 :: IO ()
main3 = printSol preferences3




--
-- Problem instances.
--
-- Note that data is 1 based and is converted to 0 based in the
-- photoProblem function.
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
