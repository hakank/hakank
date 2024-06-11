{- 
  
  Set covering problem in Curry CLP.FD

  Problem from 
  Katta G. Murty: "Optimization Models for Decision Making", page 302f
  http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
 
  10 senators making a committee, where there must at least be one 
  representative from each group:
  group:        senators:
  southern      1 2 3 4 5
  northern      6 7 8 9 10
  liberals      2 3 8 9 10
  conservative  1 5 6 7
  democrats     3 4 5 6 7 9
  republicans   1 2 8 10

  The objective is to minimize the number of senators.

  This is a port of my Picat model http://hakank.org/picat/set_covering3.pi

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (matrixElement)
import CLP.FD


set_covering3 :: [[Int]] -> [Char] -> [[Int]]
set_covering3 belongs senators = let
                                   numGroups = length belongs
                                   numSenators = length senators

                                   -- decision variables
                                   -- which senators to select
                                   xs = take numSenators (domain 0 1)
                                   -- how many (to minimize)
                                   z = head (domain 1 numSenators)
                                 in
                                   solveFDAll [] (z:xs) $
                                   z =# Data.List.sum xs /\                                   
                                   --  cover all groups with the senators
                                   foldl1 (/\) [ Data.List.sum [(xs !! j) * (fd $ matrixElement belongs i j) |
                                                      j <- [0..numSenators-1]] >=# 1
                                                 | i <- [0..numGroups-1]
                                               ]

selectedSenators :: [Int] -> [Char] -> [Char]
selectedSenators sol senators = [senators !! i | i <- [0..n-1], sol !! i == 1]
                                where
                                  n = length senators

{-
  All optimal solutions:

  opt: 2
  all optimal solutions: [[0,0,0,0,1,0,0,0,0,1],[0,0,0,0,1,0,0,1,0,0],[0,1,0,0,0,0,1,0,0,0],[0,1,0,0,0,1,0,0,0,0],[1,0,0,0,0,0,0,0,1,0]]
  Execution time: 136 msec. / elapsed: 140 msec

-}
main :: IO ()
main = putStrLn ("opt: " ++ (show opt) ++ "\nall optimal solutions: " ++ (show allOptSols))
       where
        sols = sort $ set_covering3 belongs senators
        opt = head $ head sols
        allOptSols = [ tail sol | sol <- sols, head sol == opt]

{-
  All optimal solutions, showing the selected senators 

  opt: 2
  all optimal solutions: ["ej","eh","bg","bf","ai"]
  Execution time: 132 msec. / elapsed: 144 msec.

-}
main2 :: IO ()
main2 = putStrLn ("opt: " ++ (show opt) ++ "\nall optimal solutions: " ++ (show allOptSols))
        where
          sols = sort $ set_covering3 belongs senators
          opt = head $ head sols
          allOptSols = [ selectedSenators (tail sol) senators | sol <- sols, head sol == opt]


--
-- The Belong matrix:
--
-- 1 if a senator belongs to the group, 
-- 0 if senator don't belong to the group
--
belongs :: [[Int]]
belongs =  [[1, 1, 1, 1, 1, 0, 0, 0, 0, 0],   -- 1 southern
            [0, 0, 0, 0, 0, 1, 1, 1, 1, 1],   -- 2 northern
            [0, 1, 1, 0, 0, 0, 0, 1, 1, 1],   -- 3 liberals
            [1, 0, 0, 0, 1, 1, 1, 0, 0, 0],   -- 4 conservative
            [0, 0, 1, 1, 1, 1, 1, 0, 1, 0],   -- 5 democrats
            [1, 1, 0, 0, 0, 0, 0, 1, 0, 1]]   -- 6 republicans

senators :: [Char]
senators = "abcdefghij"

