{- 
  
  Test of decompositions of circuit and circuit_path in Curry in CLP.FD


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (chunksOf)
import HakankUtilsCLPFD (elementVal,circuit, circuit_path)
import CLP.FD


--
-- Simple test: Generate all solutions
--
circuit_test1 n  = let
                   xs = take n (domain 0 (n-1))
                   
                 in
                   solveFDAll [FirstFail] xs $
                   circuit xs



-- Just one solution
circuit_test2 n  = let
                   xs = take n (domain 0 (n-1))
                   
                 in
                   solveFD [] xs $
                   circuit xs


circuit_path_test1 n  = let
                   xs = take n (domain 0 (n-1))
                   ps = take n (domain 0 (n-1))                   
                   
                 in
                   solveFDAll [FirstFail] (xs++ps) $
                   circuit_path xs ps


main = circuit_test1 5

{-
  Number of solutions for n = 1..7:

  (1,0)
  (2,1)
  (3,2)
  (4,6)
  (5,24)
  (6,120)
  (7,720)
  Execution time: 9402 msec. / elapsed: 9410 msec.
  
-}
main2 =  mapM_ print $ map (\n -> (n, length $ circuit_test1 n)) [1..7]

{-
  The circuit and the path:

  [[1,2,3,4,0],[1,2,3,4,0]]
  [[1,2,4,0,3],[1,2,4,3,0]]
  [[1,3,4,2,0],[1,3,2,4,0]]
  [[1,3,0,4,2],[1,3,4,2,0]]
  [[1,4,3,0,2],[1,4,2,3,0]]
  [[1,4,0,2,3],[1,4,3,2,0]]
  [[2,3,1,4,0],[2,1,3,4,0]]
  [[2,4,1,0,3],[2,1,4,3,0]]
  [[2,4,3,1,0],[2,3,1,4,0]]
  [[2,0,3,4,1],[2,3,4,1,0]]
  [[2,3,4,0,1],[2,4,1,3,0]]
  [[2,0,4,1,3],[2,4,3,1,0]]
  [[3,2,4,1,0],[3,1,2,4,0]]
  [[3,4,0,1,2],[3,1,4,2,0]]
  [[3,4,1,2,0],[3,2,1,4,0]]
  [[3,0,4,2,1],[3,2,4,1,0]]
  [[3,2,0,4,1],[3,4,1,2,0]]
  [[3,0,1,4,2],[3,4,2,1,0]]
  [[4,2,3,0,1],[4,1,2,3,0]]
  [[4,3,0,2,1],[4,1,3,2,0]]
  [[4,3,1,0,2],[4,2,1,3,0]]
  [[4,0,3,1,2],[4,2,3,1,0]]
  [[4,2,0,1,3],[4,3,1,2,0]]
  [[4,0,1,2,3],[4,3,2,1,0]]
  Execution time: 83 msec. / elapsed: 83 msec.

-}
main3 = mapM_ print $ map (chunksOf n) $ circuit_path_test1 n
        where n = 5