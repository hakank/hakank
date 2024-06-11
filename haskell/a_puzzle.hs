{- 
  
  "A puzzle" in Haskell

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

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
    
-}

-- import Data.List
-- import HakankUtils

--
-- List comprehensions
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

-- a_puzzle3 :: [[Int]]
-- a_puzzle5 :: (Data a, Enum a, Num a) => [(a,a,a,a,a,a,a,a,a,a,a)]

-- [(2,1,0,0,0,0,0,1,0,2,1),(2,1,0,0,0,1,0,1,0,2,1),(2,1,0,0,0,2,0,1,0,2,1)]
-- (0.15 secs, 86,348,496 bytes)
a_puzzle5 = a_puzzle5'


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



main = do
         print a_puzzle5 --(2.34 secs, 1,823,665,920 bytes)
         print a_puzzle6 --(2.32 secs, 1,830,278,016 bytes)