{-
  Wordle in Haskell

  Wordle daily puzzle: 
  https://www.nytimes.com/games/wordle/index.html

  This is a port of the methods / heuristics in my Picat program
  http://hakank.org/picat/wordle.pi

  Also see
  http://hakank.org/picat/wordle2.pi
  http://hakank.org/picat/wordle3.pi
  http://hakank.org/picat/wordle4.pi
  http://hakank.org/picat/wordle_regex.pi
  http://hakank.org/picat/wordle_dcg.pi

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/

-}

import Data.List
import Data.Char
import Data.Function
-- import HakankUtils

wordleWords = readFile "/home/hakank/curry/me/wordle_small.txt" >>= return 


--
-- correctPos xs ps
-- ps contains either '.' (unknown) or the correct character in the correct position
--
correctPos xs ps = and [p == '.' || c  == p  | (c,p) <- zip xs ps]

--
-- correctChar:
-- ps contains patterns of characters that are in the word
-- but is not in the correct position
--
correctChar _ [ ] _            = True
correctChar wd (c:cs) (pp:pps) = ((pp == "") || and [ p `elem` wd && c /=  p | p <- pp ]) &&
                                                correctChar wd cs pps 

--
-- Not in word: characters cs are not in the word xs
--
notInWord xs cs = and [not (c `elem` xs) | c <- cs]

--
-- Find the (first) position of a character in a word
--
findPos c xs = head [ p | (x,p) <- zip xs [0..], x == c]

              
--
-- Score a word
-- a) sum the places in the alphas for each position in the word.
--    The `div` 2 is a simple heuristic for - hopefully - better results.
-- b) we like words with unique characters so they get 100 points extra
--
scoreWord :: [Char] -> Int
scoreWord ws  =  sum [(1+findPos c alpha) `div` 2  | (c,alpha) <- zip ws alphas] + (allDiff ws)
                 where alphas = ["xzyjkquinovhewlrmdgfaptbcs",
                                 "jzqfkgxvbsdymcwptnhulieroa",
                                 "qjhzkxfwyvcbpmgdstlnrueoia",
                                 "jqxyzbwhfvpkmdguotcrilasne",
                                 "jqvuzxbiwfcsgmpoakdnhlrtye"]
                       -- we like words with distinct characters
                       allDiff w = if w == nub w then 100 else 0

--
-- sort the candidates according to the scores
--

-- This does not work in Haskell (but works in Curry)
-- sortCandidates ws = sortBy (\a b -> (scoreWord a) > (scoreWord b) ) ws
-- This works in Haskell but not in Curry.
sortCandidates ws = reverse $ sortBy (compare `on` scoreWord ) ws

--
-- The two Wordle word lists
--
wordleWordsSmall = fmap (words) $ readFile "/home/hakank/curry/me/wordle_small.txt"
wordleWordsLarge = fmap (words) $ readFile "/home/hakank/curry/me/wordle_large.txt"
unixDict         = fmap (filter (\w -> length w == 5 && and [isAsciiLower c | c <- w ] ) . words) $ readFile "/usr/share/dict/words" -- 4594 5 char words words (removed words with 'strange' characters)
wordsLower         = fmap (filter (\w -> length w == 5) . words) $ readFile "/home/hakank/picat/me/words_lower.txt"  -- 415834 words -> 21830  5 char words

--
-- wordle1 correctPosition correctChars notInWords
-- returns the candidate words ordered by scores
--
wordle1 cp cc niw = do ws <- wordleWordsSmall 
                       -- ws <- wordleWordsLarge
                       -- ws <- unixDict  
                       -- ws <- wordsLower
                       let matched = sortCandidates [w | w <- ws,
                                                         correctPos w cp,
                                                         correctChar w w cc,
                                                         notInWord w niw]
                       return matched


--
-- The results shown below are for the wordle_small.txt wordlist
--
test1 = wordle1 "...n." ["","","","",""] "slat" 
-- ->  [crone,brine,crony,briny,prone,corny,borne,prune,drone,phone,brink,phony,bound,pound,grind,frond,found,bring,drink,being,whine,fiend,chunk,whiny,prong,mound,horny,urine,round,drunk,irony,doing,wound,hound,opine,wring,rhino,downy,wrong,wrung,ebony,ovine,dying,young,owing,eying,vying,eking,penne,penny,bunny,funny,going,ninny,ozone,icing]

test2 = wordle1 ".r.n." ["","","","",""] "slatcoe"
-- -> [briny,brink,grind,bring,drink,drunk,wring,wrung]
  
test3 = wordle1 "...st" ["s","","","",""] "flancre"
-- -> [moist,hoist,ghost,midst,joist,joust,boost,twist]
  
test4 = wordle1  ".r.n." ["","","","",""] "slatcoe"
-- -> [briny,brink,grind,bring,drink,drunk,wring,wrung]
  
test5 = wordle1 ".r.n." ["","","","",""] "slatcoebiy"
-- -> [drunk,wrung]
  
test6 = wordle1 ".run." ["","","","",""] "slatcoebiydk"
-- ->  [wrung]

test7 = wordle1 ".l..." ["","","a","","t"] "sn"
-- -> [alter,ultra,altar]

--
-- Timing for all words (sorted by scores) for wordle_small.txt :
-- (1.22 secs, 1,664,177,992 bytes)
--
-- Timing for all words (sorted by scores) for wordle_large.txt :
-- > fmap length $ test8
-- 12972
-- (7.52 secs, 11,164,468,088 bytes)
--
-- Using wordsLower (21830 words)
-- > fmap length $ test8
-- (14.11 secs, 20,904,592,968 bytes)
-- compiled: 0.459s
test8 = wordle1 "....." ["","","","",""] ""


test9 = fmap length $ test8


main = do
          -- test1
          -- test2
          -- test3
          -- test4
          -- test5
          -- test6
          -- test7
          -- test8
          t <- test9
          print t
