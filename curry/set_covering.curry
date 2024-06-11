{- 
  
  Set covering problem in Curry CLP.FD

  Placing of firestations, from Winston "Operations Research", page 486

  This is a port of my Picat model http://hakank.org/picat/set_covering.pi

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (matrixElement)
import CLP.FD

set_covering :: (Int, [[Int]]) -> [[Int]]
set_covering (minDistance,distances) = let
                   numCities = length distances

                   -- decision variables
                   -- where to place the fire stations: 1 if placed in this city.
                   xs = take numCities (domain 0 1)
                   -- objective: minimize the number of fire stations
                   z = head (domain 0 100)
                 in
                   solveFDAll [] (z:xs) $
                   
                   -- calculate the number of covered fire stations:
                   -- there must be at least one firestation that can reach this city
                   foldl1 (/\) [ Data.List.sum [ xs !! i |
                                                 i <- [0..numCities-1],
                                                 matrixElement distances i j <= minDistance
                                                 ] >=# 1
                                 | j <- [0..numCities-1] ] /\

                   -- objective: minimize the number of fire stations
                   z =# Data.List.sum xs

{-
  opt: 2
  firestations: [0,1,0,1,0,0]
  Execution time: 33 msec. / elapsed: 36 msec.

-}
main :: IO ()
main =  let
          (opt:sol) = head . sort $ set_covering problem1
        in
          putStrLn ("opt: " ++ (show opt) ++ "\nfirestations: " ++ (show sol))

--
-- Placing of firestations, from Winston "Operations Research", page 486
--
problem1 :: (Int,[[Int]])
problem1 = (15, -- minimum distance 
            [[0,10,20,30,30,20],  -- distances between the cities
             [10,0,25,35,20,10],
             [20,25,0,15,30,20],
             [30,35,15,0,15,25],
             [30,20,30,15,0,14],
             [20,10,20,25,14,0]])
