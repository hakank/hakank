{- 
  
  Set covering problem in Curry CLP.FD

  Example 9.1-2, page 354ff, from Taha "Operations Research - An Introduction"
  Minimize the number of security telephones in street corners on a campus.

  This is a port of my Picat model http://hakank.org/picat/set_covering2.pi


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (matrixElement)
import CLP.FD


set_covering2 :: (Int, [(Int, Int)]) -> [[Int]]
set_covering2 (numStreets,corners) = let
                                       -- decision variables
                                       -- where to place the telephones
                                       xs = take numStreets (domain 0 1)
                                       -- number of placed telephones (to be minimized)
                                       z = head (domain 0 numStreets)
                                     in
                                       solveFDAll [] (z:xs) $
                                       -- Ensure that all streets are covered
                                       foldl1 (/\) [ (xs !! s1) + (xs !! s2) >=# 1 
                                                     | (s1,s2) <- corners ] /\
                                       z =# Data.List.sum xs
                                       

{-
  opt: 4
  all optimal solutions: [[0,1,0,0,1,1,1,0],[1,1,0,0,1,0,1,0]]
  Execution time: 40 msec. / elapsed: 39 msec.

-}
main :: IO ()
main = putStrLn ("opt: " ++ (show opt) ++ "\nall optimal solutions: " ++ (show allOptSols))
       where 
         sols = set_covering2 problem2
         opt = head $ head sols
         allOptSols = [ tail sol | sol <- sols, head sol == opt]


--
-- corners of each street
--
problem2 :: (Int,[(Int,Int)])
problem2 = (8,      -- number of streets
            [(0,1), -- the corners of each street
             (1,2),
             (3,4),
             (6,7),
             (5,6),
             (1,5),
             (0,5),
             (3,6),
             (1,3),
             (4,7),
             (2,4)])

