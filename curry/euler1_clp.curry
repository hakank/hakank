{-

  Euler #1 CLP in Curry.

  Problem 1
  """
  If we list all the natural numbers below 10 that are multiples of 3 or 5, 
  we get 3, 5, 6 and 9. The sum of these multiples is 23.
  Find the sum of all the multiples of 3 or 5 below 1000.
  """

  This is a CLP.FD version of euler1.

  This Curry program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Curry page: http://www.hakank.org/curry/


-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}

-- :add Data.List Control.AllValues System.CPUTime


import Control.AllValues
import Control.SetFunctions
import Data.List
import System.CPUTime
import CLP.FD as fd -- only PAKCS supports this
import HakankUtilsCLPFD

-- -- CLP.FD
-- -- Perhaps a little overkill...
-- -- mod2 a b m d
-- -- This corresponds to 
-- --   a mod b = m
-- -- d is the domain of c (the divisor of a)
-- --
-- -- Note: Only PAKCS supports this.
-- mod2 a b m d = let
--                  c = head (domain 0 d)          
--                in
--                  a =# b*c + m /\
--                  m <# b

mod3 :: [Int]
mod3 = let
          a = head (domain 1 999)
        in
          solveFD [] [a] $
          mod2 a 3 0 999

mod5 :: [Int]
mod5 = let
          a = head (domain 1 999)
        in
          solveFD [] [a] $
          mod2 a 5 0 999

euler1h :: Int
euler1h = (Data.List.sum . concat) $ (allValues mod3) `union` (allValues mod5)

-- Times is the fastest of two runs (often the second)
-- Note: The times for KICS2 compilation times are for the complete file,
-- not just a program. If testing a single program it's about 1.1s + 1.s + Execution time
main :: IO ()
main = do
         -- PACKS: Execution time: 162 msec. / elapsed: 181 msec. 
         -- KIKCS2: does not support CLP.FD
         -- Curry2Go: does not support CLP.FD
         print euler1h
