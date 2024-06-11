{- 
  
  Euler #4 in Curry

  Problem 4
  """
  A palindromic number reads the same both ways. The largest palindrome made 
  from the product of two 2-digit numbers is 9009 = 91 × 99.

  Find the largest palindrome made from the product of two 3-digit numbers.
  """

  PAKCS:
  > maximum [x*y | x <- [10..99], y <- [x..99], palin (show (x*y))]
  9009
  Execution time: 118 msec. / elapsed: 144 msec

  > maximum [x*y | x <- [100..999], y <- [x..999], palin (show (x*y))]
  906609
  Execution time: 9240 msec. / elapsed: 12875 msec.

  CP to the rescue! See euler4_clp.curry
  > euler4d
  906609
  Execution time: 76 msec. / elapsed: 78 msec


  Haskell:
  Prelude> :set +s
  Prelude> palin xs = xs == reverse xs
  Prelude> maximum [x*y | x <- [100..999], y <- [x..999], palin (show (x*y))]
  906609
  (0.28 secs, 304,226,920 bytes)

  See euler4_clp.curry for a quite fast version.

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
   
-}

import Data.List
import Control.AllValues
import Control.SetFunctions
import HakankUtils

palin :: Eq a => [a] -> Bool
palin xs = xs == reverse xs

-- max_palin :: [Int] -> Int -> Int -> Bool
-- max_palin [] _ a = a
-- max_palin (x:xs) n a = max_palin' x [x..n] a

-- max_palin' x (y:ys) a = [

{-
-- This is not correct (i.e. it does not get the correct value).
-- And it's because it's doing a BFS: It first exhaust  and then 
-- I thought that it should be quite faster than list comprehension but it is not.
-- (This uses Control.AllValues.getOneValue)
--   > getOneValue $ merge [999,998..100] [999,998..100]
--   Just 49894
--   Execution time: 9852 msec. / elapsed: 10931 msec.

-- Using setfunctions (Control.SetFunctions.selectValue) is a little faster, though still wrong
-- > selectValue $ set2 merge [999,998..100] [999,998..100]
-- (49894,494,101)
-- Execution time: 7067 msec. / elapsed: 7875 msec.

-- merge :: (Num a, Show a) => [a] -> [a] -> a
-- merge [] (y:ys) = y
-- merge (x:xs) [] = x
merge (x:xs) (y:ys) | x >= y = if palin (show t) then
                        (t,x,y)
                      else
                        merge (x:xs) ys ? merge xs (y:ys)
                      where
                        t = x*y

-- Not correct, but I want to pursue this idea more...
euler4x = oneValue $ merge [999,998..100] [999,998..100]
-}


euler4a :: Int
euler4a = maximum [x*y | x <- [100..999], y <- [x..999], palin (show (x*y))]

euler4b :: Int
euler4b = maximum [t | x <- [100..999], y <- [x..999], t <- [x*y], palin (show t)]

-- Using Applicative Function for cross product: Slow
euler4c :: Int
euler4c = maximum $ filter (\x -> palin $ show x) ( pure (*) <*> [100..999] <*> [100..999])



main :: IO ()
main = do
         -- PAKCS: Execution time: 9240 msec. / elapsed: 12875 msec  
         -- KICS2: 1.72s+1.80s+0.18s
         -- Curry2Go: Compilation time: 2.30s / elapsed: 0:01.5 Execution time: 62.21s / elapsed: 0:16.77
         print euler4a
         
         -- PAKCS: Execution time: 10455 msec. / elapsed: 13819 msec 
         -- KICS2: 2.18s+1.88s+0.20s
         -- Curry2Go: Compilation time: 2.27s / elapsed: 0:01.74 Execution time: 67.89s / elapsed: 0:17.96
         -- print euler4b
         
         -- PAKCS: Execution time: 15842 msec. / elapsed: 20403 msec  
         -- KICS2: 2.28s+1.91s+0.31s
         -- Curry2Go: Compilation time: 2.24s / elapsed: 0:01.72 Execution time: 45.61s / elapsed: 0:27.12
         -- print euler4c
         
         -- PAKCS: Execution time: 79 msec. / elapsed: 86 msec.  See euler4_clp.curry
         -- KICS2: does not support CLP.FD
         -- Curry2Go: does not support CLP.FD
         -- print euler4d
         
