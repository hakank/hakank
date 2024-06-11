{- 
  
  Euler #19 in Curry

  """
  You are given the following information, but you may prefer 
  to do some research for yourself.

  * 1 Jan 1900 was a Monday.
  * Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.
  * A leap year occurs on any year evenly divisible by 4, but not 
    on a century unless it is divisible by 400.
  
  How many Sundays fell on the first of the month during the 
  twentieth century (1 Jan 1901 to 31 Dec 2000)?
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/

-}

import Data.List
import HakankUtils
-- import Control.AllValues
-- import Control.SetFunctions
-- import CLP.FD

euler19a :: Int
euler19a = sum [ if dow year month 1 == 0 then 1 else 0 | year <- [1901..2000], month <- [0..11]]

euler19b :: Int
euler19b = length [ 1 | year <- [1901..2000], month <- [0..11], dow year month 1 == 0]

main :: IO ()
main = do
         -- PAKCS: Execution time: 123 msec. / elapsed: 136 msec.
         -- KICS2: KiCS2 compilation time: 1.13s / elapsed: 0:01.34 GHC compilation time: 1.12s / elapsed: 0:01.42 Execution time: 0.00s / elapsed: 0:00.01
         -- Curry2Go: Compilation time: 2.25s / elapsed: 0:01.62 Execution time: 0.26s / elapsed: 0:00.18
         -- print euler19a

         -- PAKCS: Execution time: 83 msec. / elapsed: 102 msec.
         -- KICS2: KiCS2 compilation time: 1.77s / elapsed: 0:02.07 GHC compilation time: 1.27s / elapsed: 0:01.72 Execution time: 0.01s / elapsed: 0:00.01
         -- Curry2Go: Compilation time: 1.98s / elapsed: 0:01.53 Execution time: 0.25s / elapsed: 0:00.18
         print euler19b
