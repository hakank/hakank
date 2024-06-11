{- 
  
  Traveling Salesperson Problem in Curry CLP.FD

  Inspired by the code from lecture notes
  Ulf Nilsson: Transparencies for the course TDDD08 Logic
  Programming, page 6f
  http://www.ida.liu.se/~TDDD08/misc/ulfni.slides/oh10.pdf

  However this version use foreach/list comprehensions.

  This is a port of my Picat model http://hakank.org/picat/tsp.pi

  Nope, this is too slow, probably due to the slow implementations
  of elementValue and circuit...

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import HakankUtilsCLPFD (circuit,elementVal)
import CLP.FD
import Debug.Trace

--
-- Way too slow...
--
tsp p = let
           len = length p
           cities = take len (domain 0 (len-1))

           dists = [d | d <- nub $ concat p, d > 0]
           minDist = minimum dists
           maxDist = maximum dists

           costs = take len (domain minDist maxDist)
           cost = head (domain (len*minDist) (len*maxDist))
           costOpt = anyOf [len*minDist..len*maxDist]
        in
           solveFD  [Bisect,Minimize costOpt] ([cost] ++ cities) $
           -- allDifferent cities /\
           traceShow (show costOpt) fd costOpt =# cost /\
           circuit cities /\
           cost =# Data.List.sum costs /\
           --  row[city] = c
           foldl1 (/\) [ elementVal city (map fd row) c | (row,city,c) <- zip3 p cities costs ]
           -- foldl1 (/\) [ elementVal (cities !! i) (map fd (p !! i)) (costs !! i) | i <- [0..len-1] ]
          
main = tsp tsp_test_nilsson


--
-- Problem from Nilsson cited above.
--
tsp_test_nilsson = [[ 0, 4, 8,10, 7,14,15],
                    [ 4, 0, 7, 7,10,12, 5],
                    [ 8, 7, 0, 4, 6, 8,10],
                    [10, 7, 4, 0, 2, 5, 8],
                    [ 7,10, 6, 2, 0, 6, 7],
                    [14,12, 8, 5, 6, 0, 5],
                    [15, 5,10, 8, 7, 5, 0]]


--
-- This problem is from the SICStus example tsp.pl
-- The "chip" example.
--
tsp_test_chip = [[0,205,677,581,461,878,345],
                 [205,0,882,427,390,1105,540],
                 [677,882,0,619,316,201,470],
                 [581,427,619,0,412,592,570],
                 [461,390,316,412,0,517,190],
                 [878,1105,201,592,517,0,691],
                 [345,540,470,570,190,691,0]]


-- This problem is from the SICStus example tsp.pl
-- The "ilog" example.
--
tsp_test_ilog = [[2,4,4,1,9,2,4,4,1,9],
                 [2,9,5,5,5,2,9,5,5,5],
                 [1,5,2,3,3,1,5,2,3,3],
                 [2,6,8,9,5,2,6,8,9,5],
                 [3,7,1,6,4,3,7,1,6,4],
                 [1,2,4,1,7,1,2,4,1,7],
                 [3,5,2,7,6,3,5,2,7,6],
                 [2,7,9,5,5,2,7,9,5,5],
                 [3,9,7,3,4,3,9,7,3,4],
                 [4,1,5,9,2,4,1,5,9,2]]


-- data from https://www.youtube.com/watch?v=A1wsIFDKqBk
-- "Lec-29 Vehicle Routeing Problem"
-- See vrp.pi
tsp_test_vrp =  [[ 0,20,18,14,16,12,19], -- depot
                 [20, 0,22,18,30,26,28], -- city 1
                 [18,22, 0,32,20,22,21], -- city 2
                 [14,18,32, 0,20,22,21], -- city 3
                 [16,30,20,20, 0,30,22], -- city 4
                 [12,26,22,22,30, 0,26], -- city 5
                 [19,28,21,21,22,26, 0]  -- city 6
                 ]




--
-- This problem is from 
-- GLPK:s example tsp.mod
-- (via http://www.hakank.org/minizinc/tsp.mzn)
-- """
-- These data correspond to the symmetric instance ulysses16 from:
-- Reinelt, G.: TSPLIB - A travelling salesman problem library.
-- ORSA-Journal of the Computing 3 (1991) 376-84;
-- http://elib.zib.de/pub/Packages/mp-testdata/tsp/tsplib 
-- 
-- The optimal solution is 6859
-- """
tsp_test_glpk = [[0,509,501,312,1019,736,656,60,1039,726,2314,479,448,479,619,150],
                 [509,0,126,474,1526,1226,1133,532,1449,1122,2789,958,941,978,1127,542],
                 [501,126,0,541,1516,1184,1084,536,1371,1045,2728,913,904,946,1115,499],
                 [312,474,541,0,1157,980,919,271,1333,1029,2553,751,704,720,783,455],
                 [1019,1526,1516,1157,0,478,583,996,858,855,1504,677,651,600,401,1033],
                 [736,1226,1184,980,478,0,115,740,470,379,1581,271,289,261,308,687],
                 [656,1133,1084,919,583,115,0,667,455,288,1661,177,216,207,343,592],
                 [60,532,536,271,996,740,667,0,1066,759,2320,493,454,479,598,206],
                 [1039,1449,1371,1333,858,470,455,1066,0,328,1387,591,650,656,776,933],
                 [726,1122,1045,1029,855,379,288,759,328,0,1697,333,400,427,622,610],
                 [2314,2789,2728,2553,1504,1581,1661,2320,1387,1697,0,1838,1868,1841,1789,2248],
                 [479,958,913,751,677,271,177,493,591,333,1838,0,68,105,336,417],
                 [448,941,904,704,651,289,216,454,650,400,1868,68,0,52,287,406],
                 [479,978,946,720,600,261,207,479,656,427,1841,105,52,0,237,449],
                 [619,1127,1115,783,401,308,343,598,776,622,1789,336,287,237,0,636],
                 [150,542,499,455,1033,687,592,206,933,610,2248,417,406,449,636,0]]

