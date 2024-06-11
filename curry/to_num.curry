{- 
  
  Test of the toNum constraint in Curry

  This is a port of my Picat model http://hakank.org/picat/to_num.pi
  

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtilsCLPFD (toNum,mod2)
import CLP.FD

-- connect a fix number with an array of digits
test1 :: [[Int]]
test1 = let
           a = take 4 (domain 0 9)
        in
           solveFDAll [] a $
           
           fd 1234 =# toNum a 10

-- connect a number with digits
test2 :: [Int]
test2 = let
           n = head (domain 0 99999999)
        in
           solveFD [] [n] $
           
           n =# toNum [3,1,4,1,5,9,2,6] 10

--
-- show all 2 digit numbers in base 11
--
test3 :: [[Int]]
test3 = let
           a = take 2 (domain 0 10)
           n = head (domain 0 (11*11))
        in
           solveFDAll [] (n:a) $
           n =# toNum a 11

--
-- Using some different constraints.
--
test4 :: [[Int]]
test4 = let
           a = take 4 (domain 0 9)
           num = head (domain 0 9999)
        in
           solveFDAll [] (num:a) $
           num =# toNum a 10 /\
           
           allDifferent a /\
           num ># 5000 /\
           mod2 num 3 1 9999 /\  -- i.e. num `mod` 3 =# 1

           (a !! 1) + (a !! 2) =# a !! 3 /\
           
           Data.List.sum a =# 22
           
-- [[1,2,3,4]]
main1 :: [[Int]]
main1 = test1

-- [31415926]
main2 :: [Int]
main2 = test2

{-
[[0,0,0],[1,0,1],[2,0,2],[3,0,3],[4,0,4],[5,0,5],[6,0,6],[7,0,7],[8,0,8],[9,0,9],[10,0,10],[11,1,0],[12,1,1],[13,1,2],[14,1,3],[15,1,4],[16,1,5],[17,1,6],[18,1,7],[19,1,8],[20,1,9],[21,1,10],[22,2,0],[23,2,1],[24,2,2],[25,2,3],[26,2,4],[27,2,5],[28,2,6],[29,2,7],[30,2,8],[31,2,9],[32,2,10],[33,3,0],[34,3,1],[35,3,2],[36,3,3],[37,3,4],[38,3,5],[39,3,6],[40,3,7],[41,3,8],[42,3,9],[43,3,10],[44,4,0],[45,4,1],[46,4,2],[47,4,3],[48,4,4],[49,4,5],[50,4,6],[51,4,7],[52,4,8],[53,4,9],[54,4,10],[55,5,0],[56,5,1],[57,5,2],[58,5,3],[59,5,4],[60,5,5],[61,5,6],[62,5,7],[63,5,8],[64,5,9],[65,5,10],[66,6,0],[67,6,1],[68,6,2],[69,6,3],[70,6,4],[71,6,5],[72,6,6],[73,6,7],[74,6,8],[75,6,9],[76,6,10],[77,7,0],[78,7,1],[79,7,2],[80,7,3],[81,7,4],[82,7,5],[83,7,6],[84,7,7],[85,7,8],[86,7,9],[87,7,10],[88,8,0],[89,8,1],[90,8,2],[91,8,3],[92,8,4],[93,8,5],[94,8,6],[95,8,7],[96,8,8],[97,8,9],[98,8,10],[99,9,0],[100,9,1],[101,9,2],[102,9,3],[103,9,4],[104,9,5],[105,9,6],[106,9,7],[107,9,8],[108,9,9],[109,9,10],[110,10,0],[111,10,1],[112,10,2],[113,10,3],[114,10,4],[115,10,5],[116,10,6],[117,10,7],[118,10,8],[119,10,9],[120,10,10]]

-}
main3 :: IO ()
main3 = mapM_ print [(a*11 + b,[a,b])  | [a,b] <- test3]

{-
[[6178,6,1,7,8],[6358,6,3,5,8],[6538,6,5,3,8],[6718,6,7,1,8],[8167,8,1,6,7],[8257,8,2,5,7],[8347,8,3,4,7],[8437,8,4,3,7],[8527,8,5,2,7],[8617,8,6,1,7]]

-}
main4 :: IO ()
main4 = mapM_ print $ test4

main :: IO ()
main = do
         print test1
         print test2
         print test3
         print test4
         