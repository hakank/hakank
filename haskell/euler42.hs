{- 
  
  Euler #42 in Haskell

  """
  The nth term of the sequence of triangle numbers is given by, 
      tn = 1/2*n*(n+1); 
  so the first ten triangle numbers are:

  1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

  By converting each letter in a word to a number corresponding to its 
  alphabetical position and adding these values we form a word value. For example, 
  the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value 
  is a triangle number then we shall call the word a triangle word.

  Using words.txt (right click and 'Save Link/Target As...'), a 16K text file 
  containing nearly two-thousand common English words, how many 
  are triangle words?
  """

  The words file is euler42_words.txt

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
import Data.Char    
import HakankUtils (splitSep,deleteAll)

triNum :: Int -> Int
triNum n = (n*(n+1) `div`2)

-- isTriNum n = x =:= anyOf [1..20] &> triNum x =:= n &> x where x free
isTriNum :: Int -> Bool
isTriNum n = not $ null [x | x <- [1..20], triNum x == n] 

calcWord :: [Char] -> Int
calcWord w = sum [1+ ord c - ord 'A' | c <- w ]

isTriWord :: [Char] -> Bool
isTriWord w = isTriNum (calcWord w)

--
-- See euler22.hs for a similar problem
--
euler42a :: IO Int
euler42a = do words <- fmap (splitSep ',' . deleteAll '\"') $ readFile "/home/hakank/curry/me/euler42_words.txt"
              let c = length $ filter isTriWord  words
              return c

main :: IO Int
main = do
         euler42a -- (0.06 secs, 29,253,168 bytes)
              

