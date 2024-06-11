{-
  Wordle in Curry

  Wordle daily puzzle: 
  https://www.nytimes.com/games/wordle/index.html

  This is a port of the methods / heuristics in my Picat program
  http://hakank.org/picat/wordle.pi

  Also see some other approaches in Picat:
  http://hakank.org/picat/wordle2.pi
  http://hakank.org/picat/wordle3.pi
  http://hakank.org/picat/wordle4.pi
  http://hakank.org/picat/wordle_regex.pi
  http://hakank.org/picat/wordle_dcg.pi

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  

-}

import Data.List
import Data.Char
import Data.Function
-- import HakankUtils
-- import Control.AllValues
-- import Debug.Trace


{-
test1 = (x =:= makeVars 5 ['a'..'z'] & x =:= ['a'?'b','c'?'d',_,_,'z']) `seq` x  where x free

-- What characters does not start a word?
-- Note the construction of (['a'..'z'] \\) . +++
test2 =  fmap ( (['a'..'z'] \\) . nub . map head . words ) $ wordleWords 

-- Why is this returning just one word (the first) from the word list?
-- {x="back"} Just "aback"
-- Because find is defined as
-- "Returns the first element `e` of a list satisfying a predicate"
test3 = fmap ( find (=:="a" ++ x) . words ) $ wordleWords  where x free

test4 = let x free in fmap ( find (=:="a" ++ x) . words ) $ wordleWords 

-- From read_test.curry
matchPattern ws pattern = w =:= anyOf ws &> w =:= pattern &> w where w free
notMatchPattern ws pattern = w =:= anyOf ws &> w /= pattern &> w where w free

-- match [] _ = True
match0 w@(x:xs) (p:ps)
   | w == [] = True
   -- | otherwise = (if p == "" then True else x == anyOf p) &&  match0 xs ps
   | otherwise = (if p == "" then True else x `elem` p) &&  match0 xs ps   

-}


--
-- correctPos xs ps
-- ps contains either '.' (unknown) or the correct character in the correct position
--
correctPos :: [Char] -> [Char] -> Bool
correctPos xs ps = and [p == '.' || c  == p  | (c,p) <- zip xs ps]

--
-- correctChar:
-- ps contains patterns of characters that are in the word
-- but are not in the correct position.
--
correctChar :: [Char] -> [Char] -> [[Char]] -> Bool
correctChar _ []      _        = True
correctChar wd (c:cs) (pp:pps) = ((pp == "") || and [ p `elem` wd && c /=  p | p <- pp ]) &&
                                 correctChar wd cs pps 

-- Alternative using list comprehension instead
correctChar2 :: [Char] -> [[Char]] -> Bool
correctChar2 wd pps = and [ (pp == "") || (and [ p `elem` wd && c /=  p | p <- pp ])
                            | (c,pp) <- zip wd pps]


--
-- Not in word: characters cs are not in the word xs
--
notInWord :: Eq a => [a] -> [a] -> Bool
notInWord xs cs = and [not (c `elem` xs) | c <- cs]

--
-- Find the (first) position of a character in a word
--
findPos :: (Eq a, Enum b, Num b) => a -> [a] -> b
findPos c xs = head [ p | (x,p) <- zip xs [0..], x == c]

--
-- Create the inverted frequency list alphas for use in scoreWords
--
-- fmap (map head . sortBy (\a b -> (length a) < (length b)) . group . sort . map (\x -> x !! 0) ) $ wordleWordsSmall


--
-- Score a word
-- a) sum the places in the alphas for each position in the word.
--    The `div` 2 is a simple heuristic for - hopefully - better results.
-- b) we like words with unique characters so they get 100 points extra
--
-- (The alphas inverted frequency lists are from my Picat program wordle2.pi)
--
scoreWord :: Integral a => [Char] -> a
scoreWord ws  =  sum [(1+findPos c alpha) `div` 2  | (c,alpha) <- zip ws alphas] + (allDiff ws)
                 where alphas = ["xzyjkquinovhewlrmdgfaptbcs",
                                 "jzqfkgxvbsdymcwptnhulieroa",
                                 "qjhzkxfwyvcbpmgdstlnrueoia",
                                 "jqxyzbwhfvpkmdguotcrilasne",
                                 "jqvuzxbiwfcsgmpoakdnhlrtye"]
                       -- we like words with distinct characters
                       allDiff w = if w == nub w then 100 else 0

--
-- sort the candidates according to the scoreWord scores
--
 -- This works in Curry but not in Haskell:
sortCandidates :: [[Char]] -> [[Char]]
sortCandidates ws = sortBy (\a b -> (scoreWord a) > (scoreWord b) ) ws
-- This works in Haskell but not in Curry:
-- sortCandidates ws = reverse $ sortBy (compare `on` scoreWord ) ws


--
-- The word lists
--

-- 2315 words
wordleWordsSmall :: IO [[Char]]
wordleWordsSmall = fmap (words) $ readFile "/home/hakank/curry/me/wordle_small.txt" 

-- 12972 words
wordleWordsLarge :: IO [[Char]]
wordleWordsLarge = fmap (words) $ readFile "/home/hakank/curry/me/wordle_large.txt" 

-- 4594 words (removed 'strange' characters)
unixDict :: IO [[Char]]
unixDict = fmap (filter (\w -> length w == 5 &&
                 and [isAsciiLower c | c <- w ] ) . words) $
                 readFile "/usr/share/dict/words" 

-- 415834 words -> 21830  5 char words
wordsLower :: IO [[Char]]
wordsLower         = fmap (filter (\w -> length w == 5) . words) $ readFile "/home/hakank/picat/me/words_lower.txt"  

--
-- wordle correctPosition correctChars notInWords
-- returns the candidate words ordered by scores
--
wordle :: [Char] -> [[Char]] -> [Char] -> IO [[Char]]
wordle cp cc niw = do ws <- wordleWordsSmall 
                      -- ws <- wordleWordsLarge
                      -- ws <- unixDict
                      -- ws <- wordsLower
                      let matched = sortCandidates [w | w <- ws,
                                                        correctPos w cp,
                                                        -- correctChar w w cc,
                                                        correctChar2 w cc,
                                                        notInWord w niw]
                      return matched

--
-- The results shown below are for the wordle_small.txt wordlist
--

test1 :: IO [[Char]]
test1 = wordle "...n." ["","","","",""] "slat" 
-- ->  [crone,brine,crony,briny,prone,corny,borne,prune,drone,phone,brink,phony,bound,pound,grind,frond,found,bring,drink,being,whine,fiend,chunk,whiny,prong,mound,horny,urine,round,drunk,irony,doing,wound,hound,opine,wring,rhino,downy,wrong,wrung,ebony,ovine,dying,young,owing,eying,vying,eking,penne,penny,bunny,funny,going,ninny,ozone,icing]

test2 :: IO [[Char]]
test2 = wordle ".r.n." ["","","","",""] "slatcoe"
-- -> [briny,brink,grind,bring,drink,drunk,wring,wrung]

test3 :: IO [[Char]]
test3 = wordle "...st" ["s","","","",""] "flancre"
-- -> [moist,hoist,ghost,midst,joist,joust,boost,twist]

test4 :: IO [[Char]]
test4 = wordle  ".r.n." ["","","","",""] "slatcoe"
-- -> [briny,brink,grind,bring,drink,drunk,wring,wrung]

test5 :: IO [[Char]]
test5 = wordle ".r.n." ["","","","",""] "slatcoebiy"
-- -> [drunk,wrung]

test6 :: IO [[Char]]
test6 = wordle ".run." ["","","","",""] "slatcoebiydk"
-- ->  [wrung]

test7 :: IO [[Char]]
test7 = wordle ".l..." ["","","a","","t"] "sn"
-- -> [alter,ultra,altar]

--
-- All words
-- 
-- Timing for all words (sorted by scores) for wordle_small.txt :
-- PAKCS: Execution time: 20424 msec. / elapsed: 22998 msec.
-- KICS2: KiCS2 compilation time: 2.99s / elapsed: 0:03.45 GHC compilation time: 1.69s / elapsed: 0:02.10 Execution time: 0.37s / elapsed: 0:00.38
-- Curry2Go: Compilation time: 1.87s / elapsed: 0:01.41 Execution time: 92.62s / elapsed: 0:40.68
-- Haskell: (1.14 secs, 1,650,034,704 bytes)
--
-- Timing for all words (sorted by scores) for wordle_large.txt:
-- > fmap length $ test8:
-- PAKCS: Execution time: 140301 msec. / elapsed: 161376 msec.
-- KICS2: KiCS2 compilation time: 1.13s / elapsed: 0:01.36 GHC compilation time: 1.62s / elapsed: 0:02.03 Execution time: 2.34s / elapsed: 0:02.37
-- Curry2Go: Compilation time: 2.41s / elapsed: 0:01.66 Execution time: 897.38s / elapsed: 4:41.07
-- Haskell: (7.61 secs, 11,164,468,120 bytes) !
--
-- Timing for all words (sorted by scores) for wordsLower
-- > fmap length $ test8:
-- PAKCS: -
-- KICS2: KiCS2 compilation time: 2.73s / elapsed: 0:03.13 GHC compilation time: 1.64s / elapsed: 0:02.03 Execution time: 5.79s / elapsed: 0:05.84
-- Curry2Go: -
-- Haskell: (14.11 secs, 20,904,592,968 bytes) ! compiled: 0.459s
--
test8 :: IO [[Char]]
test8 = wordle "....." ["","","","",""] ""

test9 :: IO Int
test9 = fmap length $ test8

main :: IO ()
main = do
          t1 <- test1
          print t1
          t2 <- test2
          print t2
          t3 <- test3
          print t3
          t4 <- test4
          print t4
          t5 <- test5
          print t5
          t6 <- test6
          print t6
          t7 <- test7
          print t7
          -- t8 <- test8
          -- print t8
          t9 <- test9
          print t9


