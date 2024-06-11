{-

  Euler #1 in Curry.

  Problem 1
  """
  If we list all the natural numbers below 10 that are multiples of 3 or 5, 
  we get 3, 5, 6 and 9. The sum of these multiples is 23.
  Find the sum of all the multiples of 3 or 5 below 1000.
  """

  For a (quite fast) CLP.FD version, see euler1_clp.curry

  This Curry program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Curry page: http://www.hakank.org/curry/


-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}

-- :add Data.List Control.AllValues System.CPUTime


import Control.AllValues
import Control.SetFunctions
import Data.List
import System.CPUTime

euler1a :: Int
euler1a = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

-- Using x `mod` (3?5) == 0 is NOT correct! It generates a huge number of solutions
-- euler1_bad = sum [x | x <- [1..999], x `mod` (3?5) == 0]

euler1b :: Int
euler1b = sum $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1..999]

mod_3_or_5 :: Int -> Bool
mod_3_or_5 x = x `mod` 3 == 0 || x `mod` 5 == 0

euler1c :: Int
euler1c = sum $ filter mod_3_or_5 [1..999]

euler1d :: Int
euler1d = sum $ union [3,6..999] [5,10..999]

euler1e :: Int
euler1e =  sum . nub $ [3,6..999] ++ [5,10..999]

-- This is quite silly...
euler1f :: Int
euler1f = sum . nub . concat . allValues . takeWhile (<1000) $ t (3?5)
          where t x = [x,x*2..]

euler1g :: Int
euler1g = sum([i | i <- [1..999], mod i 3 == 0]) + sum([i | i <- [1..999], mod i 5 == 0]) - sum([i | i <- [1..999], mod i 3 == 0 && mod i 5 ==0])

-- euler1h is CLP.FD version. See euler1_clp.curry for this

-- This is non-det so we have to get the first solution via Control.AllValues.oneValue
-- The 'default requires preprocessing via currypp (using the OPTIONS_CYMAKE directive)
euler1i' :: Integral a => a -> Bool
euler1i' n | n `mod` 3 == 0 = True
euler1i' n | n `mod` 5 == 0 = True
euler1i''default _ = False

-- Since euler1i' is non-det this returns a lot of solutions
--   Just 233168
euler1i :: Integral a => Maybe a
euler1i = oneValue . sum $ filter euler1i' [1..999]

euler1j :: Prelude.Integral a => a
euler1j =  sum . nub $ [1..999] >>= \x -> [f 5 x,f 3 x]
           where f m x = if x `mod` m == 0 then x else 0

-- Same as euler1j but with concatMap
euler1k :: Prelude.Integral a => a
euler1k =  sum . nub $ concatMap (\x -> [f 5 x,f 3 x]) [1..999]
           where f m x = if x `mod` m == 0 then x else 0


-- A deterministic version of euler1i
euler1l :: Int
euler1l = sum $ filter euler1l' [1..999]
          where
            euler1l' n
              | n `mod` 3 == 0 = True
              | n `mod` 5 == 0 = True
              | otherwise      = False



-- Note that these function are evaluated when evaluating programs.
-- Is there a way to 'escape' a function and call it later?
-- programs = [euler1a,euler1b,euler1c,euler1d,euler1e,euler1f,euler1g,euler1h,euler1i]

-- main = programs

-- Times is the fastest of two runs (often the second)
-- Note: The times for KICS2 compilation times are for the complete file,
-- not just a program. If testing a single program it's about 1.1s + 1.s + Execution time
main :: IO ()
main = do
         -- PAKCS: Execution time: 29 msec. / elapsed: 38 msec.
         -- KICS2: 1.66s 3.21s 0.00s
         -- Curry2Go: Compilation time: 1.79s / elapsed: 0:01.45 Execution time: 0.13s / elapsed: 0:00.10
         print euler1a --  <-- Fastest

         -- PACKS: Execution time: 32 msec. / elapsed: 37 msec 
         -- KIKCS2: 4.24s 3.31s0.01s
         -- Curry2Go: Compilation time: 3.33s / elapsed: 0:02.92 Execution time: 0.12s / elapsed: 0:00.10
         -- print euler1b -- 
         
         -- PACKS: Execution time: 36 msec. / elapsed: 45 msec. 
         -- KIKCS2: 4.19s 3.21s  0.00s
         -- Curry2Go: Compilation time: 3.34s / elapsed: 0:02.94 Execution time: 0.11s / elapsed: 0:00.09
         -- print euler1c 
         
         -- PACKS: Execution time: 105 msec. / elapsed: 119 msec 
         -- KIKCS2: 4.20s 3.20s 0.00s
         -- Curry2Go: Compilation time: 3.26s / elapsed: 0:02.91 Execution time: 0.29s / elapsed: 0:00.23
         -- print euler1d
         
         -- PACKS:  Execution time: 173 msec. / elapsed: 192 msec. 
         -- KIKCS2: 4.15s 3.30s 0.00s
         -- Curry2Go: Compilation time: 3.19s / elapsed: 0:02.82 Execution time: 0.52s / elapsed: 0:00.37
         -- print euler1e
         
         -- PACKS: Execution time: 195 msec. / elapsed: 222 msec.
         -- KIKCS2:  1.60s 3.16s 0.55s
         -- Curry2Go: Compilation time: 3.27s / elapsed: 0:02.88 Execution time: 0.61s / elapsed: 0:00.38
         -- print euler1f
         
         -- PACKS: Execution time: 55 msec. / elapsed: 64 msec. 
         -- KIKCS2: 4.21s 3.10s0.59s
         -- Curry2Go: Compilation time: 3.19s / elapsed: 0:02.81 Execution time: 0.23s / elapsed: 0:00.16
         -- print euler1g 
         
         -- PACKS: Execution time: 162 msec. / elapsed: 181 msec. 
         -- KIKCS2: does not support CLP.FD
         -- Curry2Go: does not support CLP.FD
         -- print euler1h
         
         -- PACKS: Execution time: 49 msec. / elapsed: 56 msec. 
         -- KIKCS2: 4.17s 3.25s  0.08s
         -- Curry2Go: 3.24s + ??? Why does this take so long? Stopped after 5 minutes...
         -- print euler1i

         -- PACKS: Execution time: 198 msec. / elapsed: 233 msec.
         -- KIKCS2: GHC compilation time: 1.28s / elapsed: 0:01.67 Execution time: 0.00s / elapsed: 0:00.02
         -- Curry2Go: Compilation time: 4.24s / elapsed: 0:06.28 Execution time: 0.68s / elapsed: 0:00.49
         -- print euler1j

         -- PACKS: Execution time: 226 msec. / elapsed: 252 msec.
         -- KIKCS2: KiCS2 compilation time: 1.84s / elapsed: 0:02.23 GHC compilation time: 2.06s / elapsed: 0:02.52 Execution time: 0.02s / elapsed: 0:00.02
         -- Curry2Go: Compilation time: 2.69s / elapsed: 0:01.69 Execution time: 0.70s / elapsed: 0:00.48
         -- print euler1k

         -- PACKS: Execution time: 41 msec. / elapsed: 48 msec.
         -- KIKCS2: KiCS2 compilation time: 4.08s / elapsed: 0:04.62 GHC compilation time: 2.33s / elapsed: 0:02.79 Execution time: 0.00s / elapsed: 0:00.01
         -- Curry2Go: Compilation time: 2.66s / elapsed: 0:01.69 Execution time: 0.11s / elapsed: 0:00.09
         -- print euler1l

