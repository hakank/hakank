{- 
  
  Least diff problem in Curry

  What is the smallest difference between two numbers X - Y
  if you must use all the digits (0..9) exactly once, i.e.
  Minimize the difference 
    ABCDE - FGHIJ


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import CLP.FD
import Debug.Trace

--
-- Using solveFDAll and sorting for getting the minimal value
-- (slower)
--
least_diff' :: [[Int]]
least_diff' = let
                [a,b,c,d,e, f,g,h,i,j] = take 10 (domain 0 9)
                all = [a,b,c,d,e, f,g,h,i,j]
                x = head (domain 10000 99999)
                y = head (domain 10000 99999)
                diff = head (domain 0 999)
             in
                solveFDAll [FirstFail,Bisect] [diff,x,y] $
                -- solveFDAll [Minimize diff] [diff,x,y] $                
                allDifferent all /\
                x =# 10000*a + 1000*b + 100*c + 10*d + e /\
                y =# 10000*f + 1000*g + 100*h + 10*i + j /\
                diff =# x - y

least_diff :: [Int]
least_diff = minimumBy (\a b -> compare (head a) (head b)) $ least_diff'


--
-- A faster version, using solveFDOne and non-det (diffOpt)
--
least_diff2 :: [Prelude.Int]
least_diff2 = let
                [a,b,c,d,e, f,g,h,i,j] = take 10 (domain 0 9)
                all = [a,b,c,d,e, f,g,h,i,j]
                x = head (domain 10000 99999)
                y = head (domain 10000 99999)
                diff = head (domain 0 999)
                diffOpt = anyOf [0..999]
             in
                solveFDOne [FirstFail,Bisect,Minimize diffOpt] [diff,x,y] $
                fd diffOpt =# diff /\
                allDifferent all /\
                x =# 10000*a + 1000*b + 100*c + 10*d + e /\
                y =# 10000*f + 1000*g + 100*h + 10*i + j /\
                diff =# x - y

main :: IO ()
main = do
          -- PAKCS:
          -- [247,50123,49876]
          -- Execution time: 681 msec. / elapsed: 682 msec.
          print least_diff

--
-- This is faster
--
main2 :: IO ()
main2 = do
          -- PAKCS:
          -- [247,50123,49876]
          -- Execution time: 206 msec. / elapsed: 205 msec
          print least_diff2


           