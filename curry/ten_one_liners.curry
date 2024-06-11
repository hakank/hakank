{-

  10 one liners in Curry.

  From 
  10 Scala One Liners to Impress Your Friends
  http://mkaz.com/solog/scala/10-scala-one-liners-to-impress-your-friends.html
  Updated Gist:
  https://gist.github.com/mkaz/d11f8f08719d6d27bab5

  Note: This program just implement some of these one liners.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Curry page: http://www.hakank.org/curry/

-}

import Data.List
import Control.AllValues

--
-- 1. Multiple Each Item in a List by 2
-- """
-- The map function takes each element in the list and applies it to the 
-- corresponding function. In this example, we take each element and multiply it by 2. 
-- This will return a list of equivalent size, compare to other examples which use 
-- reduceLeft and foldLeft those functions return only a single value not a list.
-- 
-- (1 to 10) map { _ * 2 }
-- """
--
go1 = do
        print $ map (*2) [1..10] 
        print [x*2 | x <- [1..10]] 



--
-- 2. Sum a List of Numbers
-- """
-- The most common example using reduceLeft is summing a list of 
-- numbers. This example sums the numbers 1 to 1000 using the range 
-- function to create our list of numbers and reduceLeft iterates and 
-- sum together returning a single value. Added simpler example using built-in sum function.
--
--   (1 to 1000).reduceLeft( _ + _ )
--   (1 to 1000).sum
-- """
--
go2 = do
         print $ sum L 
         print $ foldr1 (+) L
       where L= [1..1000]


--
-- 3. Verify if Exists in a String
-- """
-- This example returns a boolean if a word in a list exists in a string. 
-- I used this example for checking if a tweet contains a word I’m interested in. 
-- I suppose technically it is three lines, but the first two are just setting variables.
--    val wordList = List("scala", "akka", "play framework", "sbt", "typesafe")
--    val tweet = "This is an example tweet talking about scala and sbt."
--    (wordList.foldLeft(false)( _ || tweet.contains(_) ))
--    wordList.exists(tweet.contains)
-- """
--
-- cartesian product of lists L1 and L2
cross l1 l2 = [ (a,b) | a <- l1, b <- l2]

-- > find "curry" "This is an example tweet talking about curry and constraints."
--True
-- find _ "" = False
find x s  = _ ++ x ++ _ =:= s

go3 = do
        print $ [w | w <- wordsL, w `elem` tweetL]
        -- This works in PAKCS, but KICS2 throws ERROR: non-determinism in I/O actions occurred!        
        print $ t2
        print $ [a | (a,b) <- cross wordsL tweetL, a==b] 
        -- print t4 -- ERROR: non-determinism in I/O actions occurred!
       where
         wordsL = ["curry", "haskell", "constraints", "csp", "action rules"]
         tweet = "This is an example tweet talking about curry and constraints."
         tweetL = words tweet
         -- t2 = let w free in (w =:= anyOf wordsL & w =:= anyOf tweetL) `seq` w
         -- t4 = let w free in (w =:= anyOf wordsL & find w tweet)  `seq` w
         t2 = let w free in (w =:= anyOf wordsL & w =:= anyOf tweetL) &> w                  
         -- t4 = let w free in (w =:= anyOf wordsL &> find w tweet)  &> w

--
-- 4. Read in a File
-- """
-- This one-liner might only be impressive if you are coming from a Java 
-- background, it is pretty common now to be able to read a file in with 
-- one line of code. Here are two examples of reading in a file, one reads 
-- entire file in to a string, the other reads in each line as an entry in a List.
-- 
--    val fileText = io.Source.fromFile("data.txt").mkString
--    val fileLines = io.Source.fromFile("data.txt").getLines.toList
--
-- """
--
go4 = fmap words $ readFile "data.txt" 
          

--
-- 5. Happy Birthday to You!
-- """
-- A common one-liner which prints out the Happy Birthday song. This 
-- illustrates scala’s ternary operator as well as combining map and foreach.
--   (1 to 4).map { i => "Happy Birthday " + (if (i == 3) "dear NAME" else "to You") }.foreach { println }
-- """
--
-- hakank: How do I print the elements on each line
go5 = ["Happy Birthday " ++ if i == 3 then "dear NAME" else "to You" | i <- [1..4]]




--
-- 6. Filter list of numbers
-- """
-- Filter a list of numbers into two categories based on a criteria using partition.
-- This example creates two lists of students based on their test scores.
--      val (passed, failed) = List(49, 58, 76, 82, 88, 90) partition ( _ > 60 )
-- """
--
-- Note: Picat don't have partition/2 as a built-in...
--
-- partition([],_P) = [[],[]].
-- partition([X|Xs],P) = cond(call(P,X),
--                             [[X|Ys],Zs],
--                             [Ys,[X|Zs]]) =>
--                        [Ys,Zs] = partition(Xs,P).

-- f(X) => X > 60. -- ... nor general support for lambdas.
go6 = partition (>60) [49, 58, 76, 82, 88, 9,60]
   
--
-- 7. Fetch and Parse an XML web service
-- """
-- Since XML is a native structure to scala, parsing an XML feed comes with no effort. 
-- Here’s an example fetching the Twitter search feed.
-- """
--
--
go7 = "nope"


--
-- 8. Find minimum (or maximum) in a List
-- """
-- Another couple of examples using reduceLeft to iterate through a list and 
-- apply a function. Added simpler examples of the method min/max on the list.
--    List(14, 35, -7, 46, 98).reduceLeft ( _ min _ )
--    List(14, 35, -7, 46, 98).min
--    List(14, 35, -7, 46, 98).reduceLeft ( _ max _ )
--    List(14, 35, -7, 46, 98).max
-- """
--
go8 = do
         print $ minimum list
         print $ foldl1 min list
         print $ maximum list
         print $ foldl1 max list
         print $ allValues $ (minimum ? maximum) list
      where
         list = [14, 35, -7, 46, 98]

-- 9. Parallel Processing
-- """
-- Scala 2.9 introduced a new collection type called "parallel collections" which 
-- utilize multi-core processors when performing bulk operations such as foreach, 
-- map, filter, etc… Here’s a video of Aleksandar Prokopec explaining parallel 
-- collections at Scala Days 2010.
-- 
-- This example is not quite a copy-and-paste into the REPL, but it illustrates 
-- how to use parallel collections. Imagine you had a set of data defined in a list 
-- dataList and a function processItem which was very cpu intense. The following 
-- one-liner would give you parallel processing over the list.
-- 
--    val result = dataList.par.map( line => processItem(line) )
-- """
--
-- Currently don't support parallel data processing, or I don't know how...
--
go9 = "nope9"

-- 10. Sieve of Eratosthenes
-- """
-- Ok, this one isn’t quite practical and technically is not a one-liner since it 
-- relies on a operator being previously defined, but it is still pretty 
-- darn cool, even if it is unreadable. Daniel Sobral created the Sieve of 
-- Eratosthenes which is a algorithm used to determine if a number is prime.
-- 
--   (n: Int) => (2 to n) |> (r => r.foldLeft(r.toSet)((ps, x) => if (ps(x)) ps -- (x * x to n by x) else ps))
-- Requires definition of |> operator, a syntax borrowed from F#. See Steve Gilham’s blog for an example.
-- """
--
-- From https://wiki.haskell.org/Prime_numbers
-- This does not work
-- primes = (2 : 3 : [5,7..]) \\ (nub $ sort $ concat [[p*p, (p*p)+(2*p)..] | p <- tail primes])

-- https://wiki.haskell.org/Prime_numbers
primesTo m = eratos [2..m]               -- bounded, up to m
    where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs \\ [p,(p+p)..m])

go10 = primesTo 100



main = do
        print "go1"
        go1
        
        print "go2"
        go2
        
        print "go3"
        go3
        
        print "go4"
        go4
        
        print "go5"
        print go5
        
        print "go6"
        print go6
        
        print "go7"
        print go7
        
        print "go8"
        go8
        
        print "go9"
        print go9

        print "go10"
        print go10