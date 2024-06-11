{- 
  
   "A puzzle" in Curry

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

  The domain of the variables is 0..3.

  Note: 
  This model yields 3 solutions, since x4 is not 
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


  Also see a CLP.FD version (only PAKCS) in a_puzzle_clp.curry
  It's much faster even though the domains of the variables are 0..9
  (instead of 0..3 in the programs here).


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (makeVars)



--
-- Using free variables
--


-- PAKCS:
--    [(2,1,0,0,0,0,0,1,0,2,1),(2,1,0,0,0,1,0,1,0,2,1),(2,1,0,0,0,2,0,1,0,2,1),(2,1,0,0,0,3,0,1,0,2,1)]
--    Execution time: 42483 msec. / elapsed: 42806 msec.

-- KICS2:
--    KiCS2 compilation time: 4.88s / elapsed: 0:05.44
--    GHC compilation time: 6.78s / elapsed: 0:07.26
--    [(2,1,0,0,0,0,0,1,0,2,1),(2,1,0,0,0,1,0,1,0,2,1),(2,1,0,0,0,2,0,1,0,2,1),(2,1,0,0,0,3,0,1,0,2,1)]
--    Execution time: 64.15s / elapsed: 1:04.55
--
-- Curry2Go:
--    Compilation time: 2.38s / elapsed: 0:01.90
--    [(2, 1, 0, 0, 0, 0, 0, 1, 0, 2, 1), (2, 1, 0, 0, 0, 1, 0, 1, 0, 2, 1), (2, 1, 0, 0, 0, 2, 0, 1, 0, 2, 1)]
--    Execution time: 28.31s / elapsed: 0:04.08
--
a_puzzle3' :: (Data a, Enum a, Num a) => (a,a,a,a,a,a,a,a,a,a,a)
a_puzzle3' =  [x, x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] =:= makeVars 11 [0..3] &>
              x8+x8+x0+x9 =:= 6  &> 
              x7+x1+x1+x1 =:= 0  &> 
              x2+x1+x7+x2 =:= 0  &> 
              x6+x6+x6+x6 =:= 4  &> 
              x1+x1+x1+x1 =:= 0  &> 
              x3+x2+x1+x3 =:= 0  &> 
              x7+x6+x6+x2 =:= 2  &> 
              x9+x3+x1+x2 =:= 1  &> 
              x0+x0+x0+x0 =:= 4  &> 
              x2+x2+x2+x2 =:= 0  &> 
              x3+x3+x3+x3 =:= 0  &> 
              x5+x5+x5+x5 =:= 0  &> 
              x8+x1+x9+x3 =:= 3  &> 
              x8+x0+x9+x6 =:= 5  &> 
              x7+x7+x7+x7 =:= 0  &> 
              x9+x9+x9+x9 =:= 4  &> 
              x7+x7+x5+x6 =:= 1  &> 
              x6+x8+x5+x5 =:= 3  &> 
              x9+x8+x8+x1 =:= 5  &> 
              x5+x5+x3+x1 =:= 0  &> 

              x2+x5+x8+x1 =:= x &>
              (x, x0,x1,x2,x3,x4,x5,x6,x7,x8,x9)
              where x, x0,x1,x2,x3,x4,x5,x6,x7,x8,x9 free
               

-- a_puzzle3 :: [[Int]]
a_puzzle3 :: (Data a, Enum a, Num a) => [(a,a,a,a,a,a,a,a,a,a,a)]
a_puzzle3 = allValues $ a_puzzle3'



-- PAKCS:
--    [(2,1,0,0,0,0,0,1,0,2,1),(2,1,0,0,0,1,0,1,0,2,1),(2,1,0,0,0,2,0,1,0,2,1),(2,1,0,0,0,3,0,1,0,2,1),(3,2,1,0,0,0,0,1,0,2,0),(3,2,1,0,0,1,0,1,0,2,0),(3,2,1,0,0,2,0,1,0,2,0),(3,2,1,0,0,3,0,1,0,2,0)]
--    Execution time: 41480 msec. / elapsed: 41481 msec.

-- KICS2:
--    KiCS2 compilation time: 4.88s / elapsed: 0:05.38
--    GHC compilation time: 6.56s / elapsed: 0:06.99
--    [(2,1,0,0,0,0,0,1,0,2,1),(2,1,0,0,0,1,0,1,0,2,1),(2,1,0,0,0,2,0,1,0,2,1),(2,1,0,0,0,3,0,1,0,2,1),(3,2,1,0,0,0,0,1,0,2,0),(3,2,1,0,0,1,0,1,0,2,0),(3,2,1,0,0,2,0,1,0,2,0),(3,2,1,0,0,3,0,1,0,2,0)]
--    Execution time: 68.84s / elapsed: 1:09.04
--
-- Curry2Go:
--    Compilation time: 4.62s / elapsed: 0:02.33
--    [(2, 1, 0, 0, 0, 0, 0, 1, 0, 2, 1), (2, 1, 0, 0, 0, 1, 0, 1, 0, 2, 1), (3, 2, 1, 0, 0, 0, 0, 1, 0, 2, 0), (2, 1, 0, 0, 0, 2, 0, 1, 0, 2, 1), (2, 1, 0, 0, 0, 3, 0, 1, 0, 2, 1), (3, 2, 1, 0, 0, 1, 0, 1, 0, 2, 0), (3, 2, 1, 0, 0, 2, 0, 1, 0, 2, 0), (3, 2, 1, 0, 0, 3, 0, 1, 0, 2, 0)]
--    Execution time: 710.68s / elapsed: 2:03.78
--
a_puzzle4' = [x, x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] =:= makeVars 11 [0..3] &>
              x8+x8+x0+x9 =:= 6 &>
              x7+x6+x6+x2 =:= 2 &>
              x9+x3+x1+x2 =:= 1 &>
              x8+x1+x9+x3 =:= 3 &>
              x8+x0+x9+x6 =:= 5 &>
              x7+x7+x5+x6 =:= 1 &>
              x6+x8+x5+x5 =:= 3 &>
              x9+x8+x8+x1 =:= 5 &>

              x2+x5+x8+x1 =:= x &>
              (x, x0,x1,x2,x3,x4,x5,x6,x7,x8,x9)
              where x, x0,x1,x2,x3,x4,x5,x6,x7,x8,x9 free

a_puzzle4 = allValues $ a_puzzle4'


--
-- List comprehensions, domain 0..3
--

--
-- PAKCS
--    [(2,1,0,0,0,0,0,1,0,2,1),(2,1,0,0,0,1,0,1,0,2,1),(2,1,0,0,0,2,0,1,0,2,1),(2,1,0,0,0,3,0,1,0,2,1)]
--    Execution time: 57557 msec. / elapsed: 65927 msec.
-- 
-- KICS2:
--    KiCS2 compilation time: 1.67s / elapsed: 0:02.03
-- GHC compilation time: 1.40s / elapsed: 0:01.78
-- [(2,1,0,0,0,0,0,1,0,2,1),(2,1,0,0,0,1,0,1,0,2,1),(2,1,0,0,0,2,0,1,0,2,1),(2,1,0,0,0,3,0,1,0,2,1)]
-- Execution time: 1.03s / elapsed: 0:01.03

--
-- Curry2Go:
-- Compilation time: 4.67s / elapsed: 0:02.35
-- [(2, 1, 0, 0, 0, 0, 0, 1, 0, 2, 1), (2, 1, 0, 0, 0, 1, 0, 1, 0, 2, 1), (2, 1, 0, 0, 0, 2, 0, 1, 0, 2, 1), (2, 1, 0, 0, 0, 3, 0, 1, 0, 2, 1)]
-- Execution time: 200.92s / elapsed: 2:03.35
--
a_puzzle5' =  [(x, x0,x1,x2,x3,x4,x5,x6,x7,x8,x9) | x <- [0..ub], x0 <- [0..ub],x1 <- [0..ub],x2 <- [0..ub],x3 <- [0..ub],
                                                    x4 <- [0..ub],x5 <- [0..ub],x6 <- [0..ub],x7 <- [0..ub],x8 <- [0..ub],
                                                    x9 <- [0..ub],
                                                    x8+x8+x0+x9 == 6, 
                                                    x7+x1+x1+x1 == 0, 
                                                    x2+x1+x7+x2 == 0, 
                                                    x6+x6+x6+x6 == 4, 
                                                    x1+x1+x1+x1 == 0, 
                                                    x3+x2+x1+x3 == 0, 
                                                    x7+x6+x6+x2 == 2, 
                                                    x9+x3+x1+x2 == 1, 
                                                    x0+x0+x0+x0 == 4, 
                                                    x2+x2+x2+x2 == 0, 
                                                    x3+x3+x3+x3 == 0, 
                                                    x5+x5+x5+x5 == 0, 
                                                    x8+x1+x9+x3 == 3, 
                                                    x8+x0+x9+x6 == 5, 
                                                    x7+x7+x7+x7 == 0, 
                                                    x9+x9+x9+x9 == 4, 
                                                    x7+x7+x5+x6 == 1, 
                                                    x6+x8+x5+x5 == 3, 
                                                    x9+x8+x8+x1 == 5, 
                                                    x5+x5+x3+x1 == 0, 

                                                    x2+x5+x8+x1 == x]
                                                    where ub = 3
               

a_puzzle5 = a_puzzle5'


--
-- PAKCS:
--   [(2,1,0,0,0,0,0,1,0,2,1),(2,1,0,0,0,1,0,1,0,2,1),(2,1,0,0,0,2,0,1,0,2,1),(2,1,0,0,0,3,0,1,0,2,1),(3,2,1,0,0,0,0,1,0,2,0),(3,2,1,0,0,1,0,1,0,2,0),(3,2,1,0,0,2,0,1,0,2,0),(3,2,1,0,0,3,0,1,0,2,0)]
--   Execution time: 56417 msec. / elapsed: 65048 msec.
--
-- KICS2:
-- KiCS2 compilation time: 4.78s / elapsed: 0:05.35
-- GHC compilation time: 6.53s / elapsed: 0:07.01
-- [(2,1,0,0,0,0,0,1,0,2,1),(2,1,0,0,0,1,0,1,0,2,1),(2,1,0,0,0,2,0,1,0,2,1),(2,1,0,0,0,3,0,1,0,2,1),(3,2,1,0,0,0,0,1,0,2,0),(3,2,1,0,0,1,0,1,0,2,0),(3,2,1,0,0,2,0,1,0,2,0),(3,2,1,0,0,3,0,1,0,2,0)]
-- Execution time: 0.95s / elapsed: 0:00.97
--
-- Curry2Go:
--    Compilation time: 2.48s / elapsed: 0:01.91
--    [(2, 1, 0, 0, 0, 0, 0, 1, 0, 2, 1), (2, 1, 0, 0, 0, 1, 0, 1, 0, 2, 1), (2, 1, 0, 0, 0, 2, 0, 1, 0, 2, 1), (2, 1, 0, 0, 0, 3, 0, 1, 0, 2, 1), (3, 2, 1, 0, 0, 0, 0, 1, 0, 2, 0), (3, 2, 1, 0, 0, 1, 0, 1, 0, 2, 0), (3, 2, 1, 0, 0, 2, 0, 1, 0, 2, 0), (3, 2, 1, 0, 0, 3, 0, 1, 0, 2, 0)]
--    Execution time: 201.87s / elapsed: 2:04.48
--
a_puzzle6' =  [(x, x0,x1,x2,x3,x4,x5,x6,x7,x8,x9) | x <- [0..ub], x0 <- [0..ub],x1 <- [0..ub],x2 <- [0..ub],x3 <- [0..ub],
                                                    x4 <- [0..ub],x5 <- [0..ub],x6 <- [0..ub],x7 <- [0..ub],x8 <- [0..ub],
                                                    x9 <- [0..ub],

                                                    x8+x8+x0+x9 == 6,
                                                    x7+x6+x6+x2 == 2,
                                                    x9+x3+x1+x2 == 1,
                                                    x8+x1+x9+x3 == 3,
                                                    x8+x0+x9+x6 == 5,
                                                    x7+x7+x5+x6 == 1,
                                                    x6+x8+x5+x5 == 3,
                                                    x9+x8+x8+x1 == 5,

                                                    x2+x5+x8+x1 == x]
              where ub = 3

a_puzzle6 = a_puzzle6'



-- main :: IO ()
main = do
          a_puzzle3

          -- a_puzzle4

          -- a_puzzle5

          -- a_puzzle5
