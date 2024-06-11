{- 
  
  Utilities for CLP.FD 

  Note: CLP.FD is only supported by PAKCS.

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

module HakankUtilsCLPFD where

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import CLP.FD


--
-- elementVal ix xs val
--
--   val = xs[ix]
--
-- elementVal ensures that the ix'th element of xs is val.
-- 
-- Note: In contrast to the 'normal' element/3 in CLP(FD) this is not reversible.
-- It use non-det so it might not be as fast as a proper element constraint.
--
elementVal :: CLP.FD.FDExpr -> [CLP.FD.FDExpr] -> CLP.FD.FDExpr -> CLP.FD.FDConstr
elementVal ix xs val = let
                         n = length xs
                         i = anyOf [0..n-1]
                       in
                         fd i =# ix /\
                         val =# xs !! i



--
-- allDifferent2 xs
--
-- Ensure that all variables are distinct.
-- This constraints might be better when using 'derived' decision variables, such as
--
--   allDifferent2 [ (x !! i) - (fd i) | i <- [0..n-1]]
-- (from nqueens_clp.curry)
--
-- The built-in allDifferent complains about TODO: Show CLP.FD.FDExpr
--
allDifferent2 :: [CLP.FD.FDExpr] -> CLP.FD.FDConstr
allDifferent2 xs = foldl1 (/\) [ (xs !! i) /=# (xs !! j) | i <- [0..n-1], j <- [(i+1)..(n-1)]]
             where n = length xs



--
-- NOPE, there's no \/!
-- allDifferentExcept0 xs = allC [ ((xs !! i) =# 0 \/ (xs !! j) =# 0)
--                                 \/
--                                 ((xs !! i) ># 0 /\ (xs !! j) ># 0)
--                               | let len = ( length xs)-1, i <- [0..len], j <- [i+1..len]]
--
-- Let's try a non-det version instead.
--
allDifferentExcept0' :: CLP.FD.FDExpr -> CLP.FD.FDExpr -> CLP.FD.FDConstr
allDifferentExcept0' x y = x ># 0 /\ y ># 0 /\ x /=# y
allDifferentExcept0' x y = x =# 0 /\ y ># 0
allDifferentExcept0' x y = x ># 0 /\ y =# 0
allDifferentExcept0' x y = x =# 0 /\ y =# 0

--
-- allDifferentExcept0 xs
-- Ensure that all values in xs that are != 0 are distinct
--
allDifferentExcept0 :: [CLP.FD.FDExpr] -> CLP.FD.FDConstr
allDifferentExcept0 xs = foldl1 (/\)  [ allDifferentExcept0' x y |
                                        i <- [0..n-1], j <- [(i+1)..(n-1)],
                                        let x = xs !! i, let y = xs !! j]
                         where n = length xs


--
-- abs2 x y d
--  d = abs (x-y)
-- Note: It uses non determinism instead of a proper constraint decomposition.
--
abs2 :: CLP.FD.FDExpr -> CLP.FD.FDExpr -> CLP.FD.FDExpr ->  CLP.FD.FDConstr
abs2 x y d = x  =# y /\ d =# (fd 0)
abs2 x y d = x >=# y /\ d =# (x - y)
abs2 x y d = x  <# y /\ d =# (y - x)

--
-- toNum x base 
-- Converts a number to/from a list of integer x given a base base
--
-- Usage:
--    num =# toNum x 10
--
toNum :: Num a => [a] -> a -> a
toNum x base = Data.List.sum [ (x !! i) * base ^ (len-(i+1)) | i <- [0..(len-1)] ]
                where len = length x

--
-- mod2 a b m d
-- This corresponds to 
--   a mod b = m
-- d is the domain of c (the divisor of a)
--
mod2 :: CLP.FD.FDExpr -> CLP.FD.FDExpr -> CLP.FD.FDExpr -> Int -> CLP.FD.FDConstr
mod2 a b m d = let
                 c = head (domain 0 d)          
               in
                 a =# b*c + m /\
                 m <# b


--
-- This is a port of my Picat decomposition in
-- http://hakank.org/picat/circuit.pi
--
circuit xs = let
               n = length xs
               n1 = n-1
               zs = take n (domain 0 n1)
             in
               allDifferent xs /\
               allDifferent zs /\
               (xs !! 0) =# (zs !! 0) /\
               zs !! n1 =# 0 /\
               foldl1 (/\) [ elementVal (zs !! (i-1)) xs (zs !! i) | i <- [1..n1]]


--
-- circuit_path xs ps
--
-- As circuit xs but also including the path ps
--
-- This is a port of my Picat implementation in
-- http://hakank.org/picat/circuit.pi
--
circuit_path xs ps = let
               n = length xs
               n1 = n-1
             in
               allDifferent xs /\
               allDifferent ps /\
               (xs !! 0) =# (ps !! 0) /\
               ps !! n1 =# 0 /\
               foldl1 (/\) [ elementVal (ps !! (i-1)) xs (ps !! i) | i <- [1..n1]]


-- ensure that xs is increasing
increasingCLP :: [CLP.FD.FDExpr] -> CLP.FD.FDConstr
increasingCLP [] = true
increasingCLP [_] = true
increasingCLP (x:y:xs) = x <=# y /\ increasingCLP (y:xs) 

-- ensure that xs is decreasing
decreasingCLP :: [CLP.FD.FDExpr] -> CLP.FD.FDConstr
decreasingCLP [] = true
decreasingCLP [_] = true
decreasingCLP (x:y:xs) = x >=# y /\ decreasingCLP (y:xs) 


