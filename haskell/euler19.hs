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

  I skip this for now, since it's a lot of float/int conversion that does not
  work as expected....

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import HakankUtils

euler19a :: Int    
euler19a = sum [ if dow year month 1 == 0 then 1 else 0 | year <- [1901..2000], month <- [0..11]]

euler19b :: Int
euler19b = length [ 1 | year <- [1901..2000], month <- [0..11], dow year month 1 == 0]

main :: IO ()
main = do
         print euler19a -- (0.03 secs, 3,033,832 bytes)
         print euler19b -- (0.02 secs, 2,834,448 bytes)
