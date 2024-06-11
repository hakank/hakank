{- 
  
  "A puzzle" in Curry CLP.FD

  From "God plays dice"
  "A puzzle"
  http://gottwurfelt.wordpress.com/2012/02/22/a-puzzle/

  And the sequel "Answer to a puzzle"
  http://gottwurfelt.wordpress.com/2012/02/24/an-answer-to-a-puzzle/

  This problem instance was taken from the latter blog post.

  """
  8809 = 6
  7111 = 0
  2172 = 0
  6666 = 4
  1111 = 0
  3213 = 0
  7662 = 2
  9312 = 1
  0000 = 4
  2222 = 0
  3333 = 0
  5555 = 0
  8193 = 3
  8096 = 5
  7777 = 0
  9999 = 4
  7756 = 1
  6855 = 3
  9881 = 5
  5531 = 0

  2581 = ?
  """

  Note: 
  This model yields 10 solutions, since x4 is not 
  restricted in the constraints. 
  All solutions has x assigned to the correct result. 
  

  The problem stated in "A puzzle"
  http://gottwurfelt.wordpress.com/2012/02/22/a-puzzle/
  is
  """
  8809 = 6
  7662 = 2
  9312 = 1
  8193 = 3
  8096 = 5
  7756 = 1
  6855 = 3
  9881 = 5

  2581 = ?
  """
  This problem instance - using the same principle - yields 
  two different solutions of x, one is the same (correct) as 
  for the above problem instance, and one is not.
  This is because here both x4 and x1 are underdefined.
  
  Note: 
  This problem has another non-algebraic and - let's say - topological
  approach which yield the same solution as the first problem and one
  of the two solutions of the second problem.


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import CLP.FD


--
-- CLP.FD
--
a_puzzle1' :: [[Int]]
a_puzzle1' = let
              [x, x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] = take 11 (domain 0 9)
             in
              solveFDAll [] [x, x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] $ 
              x8+x8+x0+x9 =# 6 /\ 
              x7+x1+x1+x1 =# 0 /\ 
              x2+x1+x7+x2 =# 0 /\ 
              x6+x6+x6+x6 =# 4 /\ 
              x1+x1+x1+x1 =# 0 /\ 
              x3+x2+x1+x3 =# 0 /\ 
              x7+x6+x6+x2 =# 2 /\ 
              x9+x3+x1+x2 =# 1 /\ 
              x0+x0+x0+x0 =# 4 /\ 
              x2+x2+x2+x2 =# 0 /\ 
              x3+x3+x3+x3 =# 0 /\ 
              x5+x5+x5+x5 =# 0 /\ 
              x8+x1+x9+x3 =# 3 /\ 
              x8+x0+x9+x6 =# 5 /\ 
              x7+x7+x7+x7 =# 0 /\ 
              x9+x9+x9+x9 =# 4 /\ 
              x7+x7+x5+x6 =# 1 /\ 
              x6+x8+x5+x5 =# 3 /\ 
              x9+x8+x8+x1 =# 5 /\ 
              x5+x5+x3+x1 =# 0 /\ 

              x2+x5+x8+x1 =# x

a_puzzle1 :: [[Int]]
a_puzzle1 = [take 1 a | a <- a_puzzle1']


--
-- The alternative version with two different solutions
--
a_puzzle2' :: [[Int]]
a_puzzle2' = let
              [x, x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] = take 11 (domain 0 9)
             in
              solveFDAll [] [x, x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] $
              x8+x8+x0+x9 =# 6 /\
              x7+x6+x6+x2 =# 2 /\
              x9+x3+x1+x2 =# 1 /\
              x8+x1+x9+x3 =# 3 /\
              x8+x0+x9+x6 =# 5 /\
              x7+x7+x5+x6 =# 1 /\
              x6+x8+x5+x5 =# 3 /\
              x9+x8+x8+x1 =# 5 /\

              x2+x5+x8+x1 =# x


a_puzzle2 :: [[Int]]
a_puzzle2 = [take 1 a | a <- a_puzzle2']


main :: IO ()
main = do
          -- PAKCS: Execution time: 7 msec. / elapsed: 7 msec.
          print "a_puzzle1:"
          print a_puzzle1

          -- PAKCS: Execution time: 1 msec. / elapsed: 1 msec.
          print "a_puzzle2:"
          print a_puzzle2