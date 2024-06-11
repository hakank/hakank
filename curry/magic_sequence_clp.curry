{-
  Magic sequence problem in Curry CLP.FD

  http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob019/spec.html
  """
  A magic sequence of length n is a sequence of integers x0 . . xn-1 between 
  0 and n-1, such that for all i in 0 to n-1, the number i occurs exactly xi 
  times in the sequence. For instance, 6,2,1,0,0,0,1,0,0,0 is a magic sequence 
  since 0 occurs 6 times in it, 1 occurs twice, ...
  """

  This is a port of my Picat model http://hakank.org/picat/magic_sequence.pi

  Program created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Curry page: http://www.hakank.org/curry/

-}

import Data.List
import Control.AllValues
-- import HakankUtils
import CLP.FD
import Debug.Trace


--
-- Note that count does not take plain integers as argument, just FD vars
-- So we have to convert to FD var with (fd i)
--
check :: [CLP.FD.FDExpr] -> Int -> CLP.FD.FDConstr
check x i 
  | i == length x = true
  | otherwise     = (count (fd i) x Equ (x !! i)) /\ check x (i+1) 

magicSequence ::  Int -> [[Int]]
magicSequence n = let
                    x = take n (domain 0 (n-1))
                  in
                    solveFDAll [FirstFail,Bisect] x $
                    check x 0
                    
main :: [[Int]]
main = do
         magicSequence 4

main2 :: [[Int]]
main2 = do
          magicSequence 5

-- No solution
main3 :: [[Int]]
main3 = do
           magicSequence 6

main4 :: [[Int]]
main4 = do
          magicSequence 7

main5 :: [(Int,[[Int]])]
main5 = map (\n -> (n,magicSequence n)) [1..30]

main6 :: [[Int]]
-- PAKCS:
-- [[96,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0]]
-- Execution time: 5068 msec. / elapsed: 5112 msec.
main6 = magicSequence 100