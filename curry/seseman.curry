{- 
  
  Seseman problem in Curry

  Description of the problem:
  
  n is the length of a border
  There are (n-2)^2 "holes", i.e.
  there are n^2 - (n-2)^2 variables to find out.
 
  The simplest problem, n = 3 (n x n matrix)
  which is represented by the following matrix:
 
   a b c 
   d   e 
   f g h 
  
  Where the following constraints must hold:
 
    a + b + c = border_sum
    a + d + f = border_sum
    c + e + h = border_sum
    f + g + h = border_sum
    a + b + c + d + e + f = total_sum


  For a (Swedish) discussion of this problem, see
  "Sesemans matematiska klosterproblem samt lite Constraint Logic Programming"
  http://www.hakank.org/webblogg/archives/001084.html
  and
  Seseman's Convent Problem: http://www.hakank.org/seseman/seseman.cgi
  (using ECLiPSe CLP code)

  It was also is commented in the (Swedish) blog post
  "Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc"
  http://www.hakank.org/webblogg/archives/001209.html
  

  This is a port of my Picat model http://hakank.org/picat/seseman.pi

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import CLP.FD


--
-- Generate all solutions, no symmetry breaking
--
seseman :: [[Int]] 
seseman= let
            rowsum = 9
            total = 24
            first_num = 1

            [a,b,c,d,e,f,g,h] = take 8 (domain first_num 9)
            
          in
            solveFDAll[] [a,b,c,d,e,f,g,h] $
            a+b+c =# rowsum /\
            a+d+f =# rowsum /\
            c+e+h =# rowsum /\
            f+g+h =# rowsum /\
            a+b+c+d+e+f+g+h =# total


--
-- Generate all solutions, with symmetry breaking
--
seseman2 :: [[Int]] 
seseman2= let
            rowsum = 9
            total = 24
            first_num = 1

            [a,b,c,d,e,f,g,h] = take 8 (domain first_num 9)
            
          in
            solveFDAll[] [a,b,c,d,e,f,g,h] $
            a+b+c =# rowsum /\
            a+d+f =# rowsum /\
            c+e+h =# rowsum /\
            f+g+h =# rowsum /\
            a+b+c+d+e+f+g+h =# total /\
            -- additional constraints for uniqueness (rotation, mirror)
            a <=# h /\
            b <=# d /\
            d <=# e /\
            e <=# g


--
-- Nicer output
--
printSolution :: [Int] -> IO ()
printSolution [a,b,c,d,e,f,g,h] = do
                                    putStrLn (show [a,b,c])
                                    putStrLn (show [d,0,e])
                                    putStrLn (show [f,g,h])
                                    putStrLn "\n"

--
-- Nice output of the solutions
--

-- All solutions
main :: IO ()
main = mapM_ printSolution $ seseman


{-
   Here are the 5 "principal" solutions (i.e. with symmetry breaking)
  
   [1,3,5]
   [3,0,3]
   [5,3,1]


   [2,3,4]
   [3,0,3]
   [4,3,2]


   [3,3,3]
   [3,0,3]
   [3,3,3]


   [4,3,2]
   [3,0,3]
   [2,3,4]


   [5,3,1]
   [3,0,3]
   [1,3,5]


   Execution time: 27 msec. / elapsed: 30 msec.

-}
main2 :: IO ()
main2 = mapM_ printSolution $ seseman2


--
-- Check that there are 85 solutions.
--
main3 :: [([[Int]], [Char])]
main3 = do 
         s <- allValues seseman
         return (s, ("number of solutions: " ++ show (length s)))

--
-- Check that there are 5 solutions.
--
main4 :: [([[Int]], [Char])]
main4 = do 
         s <- allValues seseman2
         return (s, ("number of solutions: " ++ show (length s)))








