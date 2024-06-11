{- 
  
  Euler #4 in Curry CLP.FD

  Problem 4
  """
  A palindromic number reads the same both ways. The largest palindrome made 
  from the product of two 2-digit numbers is 9009 = 91 × 99.

  Find the largest palindrome made from the product of two 3-digit numbers.
  """

  This is a fast CLP.FD version, only supported by PAKCS.

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
   
-}

import Data.List
import Control.AllValues
import Control.SetFunctions
import CLP.FD -- Only PAKCS supports this
import HakankUtils

-- CLP.FD
{-
  From Picat euler4.pi
euler4g :-
  A :: 1..9,
  B :: 0..9,
  C :: 0..9,
  P #= A * 100001 + B * 10010 + C * 1100,
  D :: 100..999,
  E :: 100..999,
  E #>= D,
  P #= D * E,
  solve($[max(P),down], [A,B,C,D,E,P]),
  println(P).

-}

-- Much faster. Using [Down]
--   > euler4d
--   906609
--   Execution time: 2903 msec. / elapsed: 2922 msec
-- Using Bisect is fast!
--   > euler4d
--   906609
--   Execution time: 76 msec. / elapsed: 78 msec
-- 
-- Note: Maximize v seems to be Maximize Int, not Maximize FDExpr!
-- Here's when using SolveFD [Maximize p, Down] [p]
-- euler4.curry:107:22-107:31 Error:
--     Type error in application
--     Maximize p
     
--     Term: p
--     Inferred type: CLP.FD.FDExpr
--     Expected type: Prelude.Int
--     Types Prelude.Int and CLP.FD.FDExpr are incompatible
--      | 
--  107 |          in solveFD [Maximize p,Down] [p] $         
--      |                      ^^^^^^^^^^
-- How do I use this properly to maximize a FDExpr?
--
--
-- Order of the constraints seems to matter...
-- It's a little slower if the constraint p =# d * e is after p =# a * ... constraint
--
-- Only PAKCS supports this
euler4d' :: [Int]
euler4d' = let
            -- a = head (domain 1 9)
            [a,b,c] = take 3 (domain 0 9)
            [d,e] = take 2 (domain 100 999)
            p = head (domain 100000 999999)
         -- in solveFD [Down,Maximize p] [a,b,c,d,e,p] $
         -- in solveFD [] [p] $ -- 4s         
         -- in solveFD [Down] [p] $ -- 2.9s
         in solveFD [Bisect] [p] $ -- 78ms !
            e ># d /\
            p =# d * e /\            
            p =# a * 100001 + b * 10010 + c * 1100

-- Since I don't know how solveFD [Maximize v] work, I use
-- Control.SetFunction (set0 and maxValue)
euler4d :: Int
euler4d = head . maxValue $ set0 euler4d'

main :: IO ()
main = do
         -- PAKCS: Execution time: 79 msec. / elapsed: 86 msec.
         -- KICS2: does not support CLP.FD
         -- Curry2Go: does not support CLP.FD
         print euler4d
