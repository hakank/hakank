{- 
  
  Langford's number problem L(2,N) i in Curry CLP.FD

  Langford's number problem (CSP lib problem 24)
  http://www.csplib.org/prob/prob024/
  """
  Arrange 2 sets of positive integers 1..k to a sequence,
  such that, following the first occurence of an integer i, 
  each subsequent occurrence of i, appears i+1 indices later
  than the last. 
  For example, for k=4, a solution would be 41312432
  """
  
  * John E. Miller: Langford's Problem
    http://www.lclark.edu/~miller/langford.html
  
  * Encyclopedia of Integer Sequences for the number of solutions for each k
    http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=014552


  Note: the langford function only calculates the positions of the numbers.
  Normally the solution is also calculated in the CP model but it requires 
  the element constraint which is not supported by the CLP.FD module.
  Instead we post process the positions with getSolution.
       
  for n=4 there are 2 solutions
  position: [1 4 2 0 3 7 6 5]
  solution: [4 1 3 1 2 4 3 2]

  position: [4 0 1 2 6 3 5 7]
  solution: [2 3 4 2 1 3 1 4]

  With symmetry breaking (pos[0] < pos[1]) only the first solution is shown.

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (findPos)
import CLP.FD
import Debug.Trace

--
-- check pos i
-- Ensure that the value of i is separated by i other numbers,
-- i.e. the 'langford constraint'
-- Note that this is in the position representation.
--
check :: [CLP.FD.FDExpr] -> Prelude.Int -> CLP.FD.FDConstr
check pos i
   | i > n    = true
   | otherwise = (pos !! (i+n-1)) =# (pos !! (i-1)) + (fd (i+1)) /\ check pos (i+1)
   where
      n = (length pos) `div` 2

--
-- langford' n:
-- Get all the valid positions of the langford solutions.
--
langford' :: Prelude.Int -> [[Prelude.Int]]
langford' n = let
                n2 = n*2
                pos = take (n2) (domain 0 (n2-1))
             in
                solveFDAll [FirstFail,Enum] pos $
                allDifferent pos /\
                check pos 1 /\
                -- symmetry breaking
                (pos !! 0) <# (pos !! 1)

--
-- Convert a list of positions to a proper solution
--
getSolution :: [Prelude.Int] -> [Prelude.Int]
getSolution pos = [ s !! j  | i <- [0..n-1], let j = findPos i pos]
                  where
                     n = length pos
                     n2 = n `div` 2
                     s = concat $ replicate 2 [1..n2]

--
-- Create the positions and then post process to get the solution(s).
-- Note that there are no solutions for n which are not (0,3) mod 4
--
langford :: Prelude.Int -> [[Prelude.Int]]
langford n = if m == 0 || m== 3 then
               map getSolution $ langford' n 
             else
               [] -- no solution
             where m = n `mod` 4

main :: [[Prelude.Int]]
main = langford 4

{-
  > main2 
  [7,3,1,6,1,3,4,5,7,2,6,4,2,5]
  [7,1,4,1,6,3,5,4,7,3,2,6,5,2]
  [7,1,3,1,6,4,3,5,7,2,4,6,2,5]
  [7,4,1,5,1,6,4,3,7,5,2,3,6,2]
  [5,7,4,1,6,1,5,4,3,7,2,6,3,2]
  [1,7,1,2,6,4,2,5,3,7,4,6,3,5]
  [5,7,1,4,1,6,5,3,4,7,2,3,6,2]
  [1,7,1,2,5,6,2,3,4,7,5,3,6,4]
  [3,6,7,1,3,1,4,5,6,2,7,4,2,5]
  [5,1,7,1,6,2,5,4,2,3,7,6,4,3]
  [4,1,7,1,6,4,2,5,3,2,7,6,3,5]
  [5,6,1,7,1,3,5,4,6,3,2,7,4,2]
  [1,6,1,7,2,4,5,2,6,3,4,7,5,3]
  [4,6,1,7,1,4,5,2,6,3,2,7,5,3]
  [4,6,1,7,1,4,3,5,6,2,3,7,2,5]
  [4,5,6,7,1,4,1,5,3,6,2,7,3,2]
  [3,4,5,7,3,6,4,1,5,1,2,7,6,2]
  [1,5,1,7,3,4,6,5,3,2,4,7,2,6]
  [6,1,5,1,7,3,4,6,5,3,2,4,7,2]
  [4,1,6,1,7,4,3,5,2,6,3,2,7,5]
  [1,5,1,6,7,2,4,5,2,3,6,4,7,3]
  [1,4,1,6,7,3,4,5,2,3,6,2,7,5]
  [1,6,1,3,5,7,4,3,6,2,5,4,2,7]
  [1,5,1,6,3,7,4,5,3,2,6,4,2,7]
  [1,4,1,5,6,7,4,2,3,5,2,6,3,7]
  [1,5,1,4,6,7,3,5,4,2,3,6,2,7]
  Execution time: 93 msec. / elapsed: 97 msec.

-}
main2 :: Prelude.IO ()
main2 = mapM_ (print) $ langford 7

{- 
  The number of solutions for n = 1..14 (with symmetry breaking)

  > main3
  (1,0)
  (2,0)
  (3,1)
  (4,1)
  (5,0)
  (6,0)
  (7,26)
  (8,150)
  (9,0)
  (10,0)
  (11,17792)
  (12,108144)
  (13,0)
  (14,0)
  Execution time: 8423 msec. / elapsed: 8486 msec.

-}
main3 :: Prelude.IO ()
main3 = mapM_ (\n -> print (n, length $ langford n)) [1..14]

