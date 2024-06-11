{- 
  
  Set covering and set partition in Curry CLP.FD

  Example from Lundgren, Ronnqvist, Varbrand "Optimeringslora", page 408.
  [This is a Swedish book about Operational Research.]
  
  We want to minimize the cost of the alternatives which covers all the 
  objects, i.e. all objects must be choosen. The requirement is than an object 
  may be selected _exactly_ once.
 
  Alternative        Cost        Object
  1                  19           1,6
  2                  16           2,6,8
  3                  18           1,4,7
  4                  13           2,3,5
  5                  15           2,5
  6                  19           2,3
  7                  15           2,3,4
  8                  17           4,5,8
  9                  16           3,6,8
  10                 15           1,6,7
 
  The problem has a unique solution of z = 49 where alternatives 
  3, 5, and 9 is selected. 
 
  If we, however, allow that an object is selected more than one time, 
  then the solution is z = 45 (i.e. less cost than the first problem),
  and the alternatives 4, 8, and 10 is selected, where object 5 is 
  selected twice (alt. 4 and 8). It's an unique solution as well.
 

  This is a port of my Picat model http://hakank.org/picat/set_covering4.pi

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (matrixElement)
import CLP.FD


set_covering4 :: ([Int], [[Int]]) -> [Char] -> [[Int]]
set_covering4 (costs,alternatives) mode =
     let
       numAlternatives = length alternatives
       numObjects      = length (head alternatives)
       sumCosts        = Data.List.sum costs
       -- decision variables
       xs = take numAlternatives (domain 0 1)
       z = head (domain 0 sumCosts)
     in
       solveFDAll [] (z:xs) $
       scalarProduct (map fd costs) xs Equ z /\
       -- set partition:  sum == 1
       -- set covering:   sum >= 1
       foldl1 (/\) [ if mode == "set partition" then sum' =# 1 else sum' >=# 1
                     | j <- [0..numObjects-1],
                       let sum' = Data.List.sum [ (xs !! i) * (fd (matrixElement alternatives i j))
                                                  | i <- [0..numAlternatives-1]]
                    ]

test_covering :: ([Int],[[Int]]) -> [Char] -> IO ()
test_covering problem mode = print $ splitAt 1 . head . sort $ set_covering4 problem mode


{-
  set covering:
  ([45],[0,0,0,1,0,0,0,1,0,1])

  set partition:
  ([49],[0,0,1,0,1,0,0,0,1,0])
  Execution time: 108 msec. / elapsed: 112 msec.

-}
main :: IO ()
main = do
         putStrLn "set covering:"
         test_covering problem "set covering"
         putStrLn "\nset partition:"
         test_covering problem "set partition"         



--
-- cost and alternatives
--
problem :: ([Int], [[Int]])
problem = ([19, 16, 18, 13, 15, 19, 15, 17, 16, 15], -- costs
           [[1,0,0,0,0,1,0,0],   -- alternative 1    -- alternatives 
            [0,1,0,0,0,1,0,1],   -- alternative 2
            [1,0,0,1,0,0,1,0],   -- alternative 3
            [0,1,1,0,1,0,0,0],   -- alternative 4
            [0,1,0,0,1,0,0,0],   -- alternative 5
            [0,1,1,0,0,0,0,0],   -- alternative 6
            [0,1,1,1,0,0,0,0],   -- alternative 7
            [0,0,0,1,1,0,0,1],   -- alternative 8
            [0,0,1,0,0,1,0,1],   -- alternative 9
            [1,0,0,0,0,1,1,0]]   -- alternative 10
          )