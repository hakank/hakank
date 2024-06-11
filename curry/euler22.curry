{- 
  
  Euler #22 in Curry

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
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Data.Char
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (deleteAll, splitSep)
-- import CLP.FD

sumUpperChars :: [Char] -> Int
sumUpperChars [] = 0
sumUpperChars (x:xs) = (ord x - ord 'A' + 1)  + sumUpperChars xs

euler22a :: IO Int
euler22a = do names <- fmap (sort . splitSep ',' . deleteAll '\"' ) $ readFile "/home/hakank/curry/me/euler22_names.txt"
              let sums = fmap (sumUpperChars) $ names
              let z = sum [ix*s | (ix,s) <- zip [1..length$names] $ sums]
              return z

--
-- From https://wiki.haskell.org/Euler_problems/21_to_30
-- This works, but I just want the idea how use do, let etc.
-- And it's more elegant than my version, especially how to read the file with read $ ...
-- Interestingly, this is slower than my version in both curry and - especially - KiCS2
--
problem_22 :: IO ()
problem_22 =
    do input <- readFile "/home/hakank/curry/me/euler22_names.txt"
       let names = sort $ read$"["++ input++"]"
       let scores = zipWith score names [1..]
       print . sum $ scores
  where score w i = (i *) . sum . map (\c -> ord c - ord 'A' + 1) $ w


main :: IO Int
main = do
         -- PAKCS: Execution time: 1760 msec. / elapsed: 2074 msec. 
         -- KICS2: 1.26s 1.37s 0.09s
         -- Curry2Go: Compilation time: 1.93s / elapsed: 0:01.51 Execution time: 9.37s / elapsed: 0:03.71
         euler22a
         
         -- PAKCS: Execution time: 2281 msec. / elapsed: 2672 msec. 
         -- KICS2: 1.11s 1.33s 0.27s
         -- Curry2Go: Compilation time: 1.99s / elapsed: 0:01.65 Execution time: 12.19s / elapsed: 0:04.56
         -- problem_22