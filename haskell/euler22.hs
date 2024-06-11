{- 
  
  Euler #22 in Haskell

  """
  Using names.txt (right click and 'Save Link/Target As...'), a 46K 
  text file containing over five-thousand first names, begin by sorting 
  it into alphabetical order. Then working out the alphabetical value 
  for each name, multiply this value by its alphabetical position in the 
  list to obtain a name score.

  For example, when the list is sorted into alphabetical order, COLIN, 
  which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in 
  the list. So, COLIN would obtain a score of 938 53 = 49714.

  What is the total of all the name scores in the file?")
  """


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
import Data.Char
-- import HakankUtils

--
-- Split a string with separator character c
--
splitSep sep s = words [if c == sep then ' ' else c | c <- s]

--
-- Delete all occurrences of character c from string a
--
deleteAll c a@(x:xs)
  | a == [] || a == [c]   = []
  | otherwise = if x == c then deleteAll c xs else x : deleteAll c xs

sumUpperChars [] = 0
sumUpperChars (x:xs) = (ord x - ord 'A' + 1)  + sumUpperChars xs

euler22a = do names <- fmap (sort . splitSep ',' . deleteAll '\"' ) $ readFile "euler22_names.txt"
              let sums = fmap (sumUpperChars) $ names
              let z = sum [ix*s | (ix,s) <- zip [1..length$names] $ sums]
              return z

--
-- From https://wiki.haskell.org/Euler_problems/21_to_30
-- This works, but I just want the idea how use do, let etc.
-- And it's more elegant than my version, especially how to read the file with read $ ...
-- Interestingly, this is slower than my version in both curry and - especially - KiCS2
-- 
problem_22 =
    do input <- readFile "euler22_names.txt"
       let names = sort $ read$"["++ input++"]"
       let scores = zipWith score names [1..]
       print . sum $ scores
  where score w i = (i *) . sum . map (\c -> ord c - ord 'A' + 1) $ w


main = do
         euler22a -- (0.12 secs, 50,398,696 bytes)
         -- problem_22 -- (0.06 secs, 95,049,160 bytes)

         