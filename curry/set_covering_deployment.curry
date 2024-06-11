{- 
  
  Set covering deployment in Curry

  From http://mathworld.wolfram.com/SetCoveringDeployment.html
  """
  Set covering deployment (sometimes written "set-covering deployment"
  and abbreviated SCDP for "set covering deployment problem") seeks 
  an optimal stationing of troops in a set of regions so that a 
  relatively small number of troop units can control a large 
  geographic region. ReVelle and Rosing (2000) first described 
  this in a study of Emperor Constantine the Great's mobile field 
  army placements to secure the Roman Empire.
  """

  This is a port of my Picat model http://hakank.org/picat/set_covering_deployment.pi

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (chunksOf)
import CLP.FD

problem :: Prelude.Num a => [[a]]
problem = [[0, 1, 0, 1, 0, 0, 1, 1],
           [1, 0, 0, 1, 0, 0, 0, 0],
           [0, 0, 0, 0, 1, 1, 0, 0],
           [1, 1, 0, 0, 0, 0, 1, 0],
           [0, 0, 1, 0, 0, 1, 1, 0],
           [0, 0, 1, 0, 1, 0, 1, 1],
           [1, 0, 0, 1, 1, 1, 0, 1],
           [1, 0, 0, 0, 0, 1, 1, 0]]

armies :: [[Prelude.Char]]
armies = ["Alexandria", "Asia Minor", "Britain", "Byzantium", "Gaul", "Iberia", "Rome", "Tunis"]

-- For some reason Curry insists of this type. Why?
-- I would have thought that it would be:
-- set_covering_deployment :: [[Int]] -> [[Int]]
set_covering_deployment :: [[CLP.FD.FDExpr]] -> [[Int]]
set_covering_deployment pp = let
                                     n = length pp
                                     
                                     xs = take n (domain 0 1) -- first army
                                     ys = take n (domain 0 1) -- second army
                                     z  = head (domain 0 100)
                                  in
                                     solveFDAll [] ([z] ++ xs ++ ys) $
                                     -- total number of armies (to be minimized)
                                     z =# Data.List.sum [x + y | (x,y) <- zip xs ys] /\

                                     -- Constraint 1: There is always an army in a city (+ maybe a backup)
                                     --               Or rather: Is there a backup, there must be an
                                     --               an army
                                     foldl1 (/\) [  x >=# y | (x,y) <- zip xs ys] /\

                                     -- Constraint 2: There should always be an backup army near every city
                                     foldl1 (/\) [ x + Data.List.sum [y*r | (y,r) <- zip ys row] >=# 1
                                                  | (x,row) <- zip xs pp ]

getSol :: [Int] -> [String]
getSol xs = [ armies !! i | i <- [0..len-1], xs !! i == 1]
            where len = length xs

{-

  opt: 4
  All optimal solutions:
  (["Byzantium","Iberia"],["Byzantium","Iberia"])
  (["Asia Minor","Iberia"],["Asia Minor","Iberia"])
  (["Asia Minor","Britain","Rome"],["Rome"])
  (["Alexandria","Iberia"],["Alexandria","Iberia"])
  (["Alexandria","Gaul"],["Alexandria","Gaul"])
  (["Alexandria","Britain"],["Alexandria","Britain"])
  Execution time: 596 msec. / elapsed: 663 msec.

-}
main :: IO ()
main =  do
          putStrLn ("opt: " ++ (show opt))
          putStrLn "All optimal solutions:"
          mapM_ print allOptSols
        where
          sols = sort $ set_covering_deployment problem
          (opt:_) = head sols
          allOptSols = [ (getSol xs,getSol ys) | sol <- sols, head sol == opt,
                         let [xs,ys] = chunksOf (length problem) (tail sol)
                         ]
