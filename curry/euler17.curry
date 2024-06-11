{- 
  
  Euler #17 in Curry

  """
  If the numbers 1 to 5 are written out in words: one, two, three, four, five, 
  then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
  
  If all the numbers from 1 to 1000 (one thousand) inclusive were written out in 
  words, how many letters would be used?
  
  NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) 
  contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of 
  "and" when writing out numbers is in compliance with British usage.
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

-- import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
-- import CLP.FD

--
-- This is a port of my SICStus Prolog/SWI Prolog solutions
--
digit :: Int -> [Char]
digit 1 = "one" 
digit 2 = "two" 
digit 3 = "three" 
digit 4 = "four" 
digit 5 = "five" 
digit 6 = "six" 
digit 7 = "seven" 
digit 8 = "eight" 
digit 9 = "nine" 

teens :: Int -> [Char]
teens 10 = "ten" 
teens 11 = "eleven" 
teens 12 = "twelve" 
teens 13 = "thirteen" 
teens 14 = "fourteen" 
teens 15 = "fifteen" 
teens 16 = "sixteen" 
teens 17 = "seventeen" 
teens 18 = "eighteen" 
teens 19 = "nineteen" 

tens :: Int -> [Char]
tens 20 = "twenty" 
tens 30 = "thirty" 
tens 40 = "forty" 
tens 50 = "fifty" 
tens 60 = "sixty" 
tens 70 = "seventy" 
tens 80 = "eighty" 
tens 90 = "ninety" 

hundred :: Int -> [Char]
hundred 100 = "onehundred"

thousand :: Int -> [Char]
thousand 1000 = "onethousand" 

spell :: Int -> [Char]
-- fix
spell 0 = ""

-- 1..10
spell n | n > 0 && n < 10 = concat [digit n]

-- 10..19
spell n | n > 9 && n < 20 = concat [teens n]

-- 20..99
spell n | n >= 20 && n < 100 = concat [ten, one]
          where
          d = 10* (n `div` 10)
          ten = tens d
          m = n `mod` 10
          one = if m > 0 then digit m else ""

-- 100.999
spell n | n >= 100 && n < 1000 = concat [hundred1, "hundred", andstr, ones]
          where
          hundred_ = n `div` 100
          hundred1 = digit hundred_
          m = n `mod` 100
          ones = spell m
          andstr = if m > 0 then "and" else ""

-- 1000
spell 1000 = "onethousand"


euler17a :: Int
euler17a = length $ concat [spell n | n <- [1..1000]]

main :: IO ()
main = do
          -- PACKS: Execution time: 97 msec. / elapsed: 117 msec. 
          -- KICS2:  0.98s 1.54s  69.86s!!!
          -- Curry2Go: Compilation time: 2.23s / elapsed: 0:01.60 Execution time: 284.45s / elapsed: 1:37.95
          print euler17a

