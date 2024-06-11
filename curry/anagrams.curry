{- 
  
  Anagrams in Curry

  This version checks for the largest sets of anagrams from a word list,
  (default /usr/dict/words). Note: this program just cares about words
  consisting of a-z.

  Also compare with the Rosetta Code:
  http://rosettacode.org/wiki/Anagrams


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
-- import CLP.FD

--
-- Remove all words that contains non a..z chars
--
allAZ :: String -> Bool
allAZ = all (\c -> c `elem` ['a'..'z'])

anagram :: [Char] ->  IO [[String]]
anagram wordList = do 
         ws <- fmap (filter allAZ . words ) $ readFile wordList 
         let ws2 = map (\w -> (length w,w)) . groupBy (\a b -> (fst a) == (fst b)) . sort $ map (\w -> (sort w,w)) ws
         -- Get the largest set of words
         -- let opt = fst $ last $ sort ws2
         -- Faster
         let opt = fst $ maximumBy (\a b -> compare (fst a) (fst b) ) ws2
         let sols = [ map snd $ snd sol | sol <- ws2, fst sol == opt]
         return $ sols

{-
  * /usr/share/dict/words: 102401 words
    [["carets","caster","caters","crates","reacts","recast","traces"],["pares","parse","pears","rapes","reaps","spare","spear"]]
    PAKCS: Execution time: 66932 msec. / elapsed: 79251 msec.
    KICS2: Execution time: 2.16s / elapsed: 0:02.29
    Curry2Go: Execution time: 587.18s / elapsed: 3:47.91

-}
main :: IO [[String]]
main = anagram "/usr/share/dict/words"

{-
  * unixdict.txt: 25104 words
    [["abel","able","bale","bela","elba"],["caret","carte","cater","crate","trace"],["angel","angle","galen","glean","lange"],["alger","glare","lager","large","regal"],["elan","lane","lean","lena","neal"],["evil","levi","live","veil","vile"]]
    PAKCS: Execution time: 33083 msec. / elapsed: 37864 msec
    KICS2: Execution time: 0.83s / elapsed: 0:00.85
    Curry2Go: Execution time: 245.50s / elapsed: 1:10.26
-}
main2 :: IO [[String]]
main2 = anagram "/home/hakank/curry/me/unixdict.txt"