{-
  Fibonacci (and memoization)

  fib1 is very slow (as expected)
  fib is quite fast

  > fib x =:= 10946 & x =:= makeChoice [1..25] where x free
  {x=21} True


  Testing fix factorial:
  Using fix.  From https://en.wikibooks.org/wiki/Haskell/Fix_and_recursion
  This works: But it's fac, not fib:-)
  > fix (\rec n -> if n == 0 then 1 else n * rec (n-1)) 5
  120
  And it's actually fast:
  -- Factorial
  > fix (\rec n -> if n == 0 then 1 else n * rec (n-1)) 100
93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
Execution time: 6 msec. / elapsed: 10 msec.

  Testing with fibonacci. Using fix is faster, but quite slow: n=30 takes about 7.5s, but it's faster than fib1 30: about 17s!
This is PACKS. KICS2 is much faster (see below)
> fix (\rec n -> if n == 0 then 0 else if n == 1 then 1 else rec (n-1) +rec (n-2)) 30
832040
Execution time: 6906 msec. / elapsed: 7560 msec.
> fib1 30
832040
Execution time: 12457 msec. / elapsed: 17102 msec.

Compare with Haskell/GHCi which is much faster than PAKCS (but KiCS2 is faster after all compilation steaps, see below).
$ ghci
Prelude>:add Control.Monad.Fix
Prelude Control.Monad.Fix>:set +s 
Prelude Control.Monad.Fix> (fix (\rec n -> if n == 1 || n == 2 then 1 else rec (n-1) + rec (n-2))) 30
832040
(0.64 secs, 407,676,944 bytes)



  However using it as a names function (fib2 30) is much slower:
  > fib2 30
  832040
  Execution time: 17306 msec. / elapsed: 20152 msec.


  This is faster, using if n==1 || n==2 ...
> (fix (\rec n -> if n == 1 || n == 2 then 1 else rec (n-1) + rec (n-2))) 30
832040
Execution time: 4370 msec. / elapsed: 4844 msec.

 But - again - using it as a (named) function is slower (faster than fib2 though)
 > fib3 30
 832040
 Execution time: 10549 msec. / elapsed: 12145 msec.

  The infinitial list version is fairly fast:

  fibonacci> length $ take 1000 $ fib_inf 
  1000
  Execution time: 114 msec. / elapsed: 671 msec.

-}

import Data.Function
import Data.List
-- import Data.Map
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Memoize

-- import HakankUtils

-- Very slow.
-- Testing fib1 30
-- PAKCS: Execution time: 13793 msec. / elapsed: 20279 msec.
-- KICS2: 
--   KiCS2 compilation time: 1.24s / elapsed: 0:01.48
--   GHC compilation time: 1.13s / elapsed: 0:01.51
--   Execution time: 24.75s / elapsed: 0:25.19
-- Curry2Go: >20min
-- 
fib1 0 = 0
fib1 1 = 1
fib1 n | n > 1 = fib1 (n-1) + fib1 (n-2)

-- From https://en.wikibooks.org/wiki/Haskell/Fix_and_recursion
--- fib2 nn = fix $ fib1 nn
-- Testing fib2 30
-- PAKCS: Execution time: 19944 msec. / elapsed: 23618 msec.
-- KICS2:
--   KiCS2 compilation time: 1.64s / elapsed: 0:01.90
--   GHC compilation time: 1.92s / elapsed: 0:02.27
--   Execution time: 0.43s / elapsed: 0:00.46
-- Curry2Go:
--   Compilation time: 1.87s / elapsed: 0:01.49
--   Execution time: 170.82s / elapsed: 0:38.07
--
--
-- Testing with fix directly is faster (as mentioned above)
-- > :add Data.Function
-- > (fix (\rec n -> if n == 1 || n == 2 then 1 else rec (n-1) + rec (n-2))) 30
-- PAKCS: Execution time: 4942 msec. / elapsed: 5470 msec.
-- KICS2:
--   KiCS2 compilation time: 1.78s / elapsed: 0:02.06
--   GHC compilation time: 2.24s / elapsed: 0:02.66
--   Execution time: 0.25s / elapsed: 0:00.30
-- Curry2Go:
--   Compilation time: 1.66s / elapsed: 0:01.38
--   Execution time: 168.43s / elapsed: 0:37.30
--
fib2 = fix (\rec n -> if n == 0 then 0 else if n == 1 then 1 else rec (n-1) + rec (n-2))


-- A little faster than fib2, but still slow
-- Testing fib3 30
-- PAKCS: Execution time: 12221 msec. / elapsed: 14234 msec.
-- KICS2:
--   KiCS2 compilation time: 1.11s / elapsed: 0:01.38
--   GHC compilation time: 1.13s / elapsed: 0:01.52
--   Execution time: 0.27s / elapsed: 0:00.29
-- Curry2Go:
--   Compilation time: 2.29s / elapsed: 0:01.69
--   Execution time: 102.53s / elapsed: 0:23.41


-- Testing with fix directly.
-- > :add Data.Function
-- > (fix (\rec n -> if n == 1 || n == 2 then 1 else rec (n-1) + rec (n-2))) 30
-- PAKCS: Execution time: 5070 msec. / elapsed: 5714 msec.
-- KICS2:
--   KiCS2 compilation time: 1.83s / elapsed: 0:02.12
--   GHC compilation time: 2.21s / elapsed: 0:02.64
--   Execution time: 0.18s / elapsed: 0:00.21
-- Curry2Go:
--   Compilation time: 2.42s / elapsed: 0:01.80
--   Execution time: 29.23s / elapsed: 0:08.28 <---Much faster than "fib3 30"!!!
--
fib3 = (fix (\rec n -> if n == 1 || n == 2 then 1 else rec (n-1) + rec (n-2)))


-- From https://en.wikibooks.org/wiki/Haskell/Fix_and_recursion
-- myfix :: (a -> a) -> a
-- myfix f = let {x = f x} in x

--
-- https://kseo.github.io/posts/2017-01-14-memoization-in-hasekll.html
-- Note that the memoized function must have an extra first parameter f to be able to work with memoize
-- This works but it very slow:
--   > map fibmemo [1..30]
--   [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040]
--   Execution time: 46189 msec. / elapsed: 54033 msec.
--
-- KICS2 is - as usual - quite faster
-- > map fibmemo [1..30]
-- KiCS2 compilation time: 1.25s / elapsed: 0:01.55
-- GHC compilation time: 1.11s / elapsed: 0:01.50
-- [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040]
-- Execution time: 1.02s / elapsed: 0:01.09


-- This is potentially non-det
-- fib4 f 0 = 0
-- fib4 f 1 = 1
-- fib4 f n | n > 1 = fib4 f (n-1) + fib4 f (n-2)
-- Deterministic version:
fib4 f n = if n == 0 then
             0
           else if n == 1 then
             1
           else fib4 f (n-1) + fib4 f (n-2)

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)
fibmemo = fix (memoize . fib4)

-- From https://wiki.haskell.org/Memoization
-- It works but it's slower!
memoized_fib :: Int -> Int
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)

-- Much faster
fiblist x y = x : fiblist y (x+y)
fib n = fiblist 0 1 !! n

-- Infinite list
fib_inf :: Num n => [n]
fib_inf = 0 : nxt
    where nxt = 1 : zipWith (+) fib_inf nxt


--
-- From https://gist.github.com/beala/d871ae8397167e7035f218a25ddf87dd
--
-- Memoizing using Data.Map and the State monad (in the transformers package)
-- This work but it's forbiddingly slow:
--   lazyMemoFibs 30 takes about 30s in KICS2 and that's the fastest system!
-- Some minutes later: after fixing the non-det fib/1 in lazyMemoFibs it now takes 0.71s.
-- So it might be usable after all...
--
-- Non-det
-- stateMemoFibs :: Int -> State (Map.Map Int Int) Int
-- stateMemoFibs 0 = return 0
-- stateMemoFibs 1 = return 1
-- stateMemoFibs n = do
--   -- Try and get the n-2 and n-1 fib from the cache. If they're not there, 
--   -- calculate them recursively and update the cache.
--   n2 <- getOrUpdate (n-2) (stateMemoFibs (n-2))
--   n1 <- getOrUpdate (n-1) (stateMemoFibs (n-1))
--   return (n2 + n1)

-- Make it deterministic
stateMemoFibs :: Int -> State (Map.Map Int Int) Int
stateMemoFibs n
  | n == 0 = return 0
  | n == 1 = return 1
  | otherwise = do
                  -- Try and get the n-2 and n-1 fib from the cache. If they're not there, 
                  -- calculate them recursively and update the cache.
                  n2 <- getOrUpdate (n-2) (stateMemoFibs (n-2))
                  n1 <- getOrUpdate (n-1) (stateMemoFibs (n-1))
                  return (n2 + n1)

lazyMemoFibs :: Int -> Int
lazyMemoFibs = (fmap fibs [0 ..] !!)
  where
    -- potentially non det
    -- fibs 0 = 0
    -- fibs 1 = 1
    -- fibs n = lazyMemoFibs (n-2) + lazyMemoFibs (n-1)
    -- Ah, when fixing it to deterministic it's much faster:
    --   KICS2 now solve lazyMemoFibs 30 in 0.71s instead of about 30s!
    fibs n
      | n == 0 = 0
      | n == 1 = 1
      | otherwise = lazyMemoFibs (n-2) + lazyMemoFibs (n-1)





-- -- Solution suggested by ChatGPT-4
-- -- Nope, it does not work.
-- fibonacci :: Int -> Int
-- fibonacci = (map fib [0..] !!) . fromIntegral
--     where
--         fib 0 = 0
--         fib 1 = 1
--         fib n = memo !! (n - 2) + memo !! (n - 1)
--         memo = Map.fromList . zip [0..] $ map fib [0..]



-- PAKCS: Execution time: 12 msec. / elapsed: 12 msec.
-- KICS2:
--   KiCS2 compilation time: 1.15s / elapsed: 0:01.49
--   GHC compilation time: 1.89s / elapsed: 0:02.34
--   55
--   Execution time: 0.00s / elapsed: 0:00.01
-- Curry2Go:
--   Compilation time: 2.41s / elapsed: 0:01.65
--   Execution time: 0.09s / elapsed: 0:00.03
--
main1 = fib1 10


-- main2 = fibmemo 10
-- PAKCS: Execution time: 5 msec. / elapsed: 5 msec.
-- KICS2:
--   KiCS2 compilation time: 1.30s / elapsed: 0:01.54
--   GHC compilation time: 1.10s / elapsed: 0:01.46
--   26925748508234281076009
--   Execution time: 0.00s / elapsed: 0:00.01
-- Curry2Go: Cannot handle large numbers
-- 
main2 = fib 109

-- PAKCS:Execution time: 11040 msec. / elapsed: 12968 msec.
-- KICS2:
--   KiCS2 compilation time: 1.22s / elapsed: 0:01.52
--   GHC compilation time: 1.16s / elapsed: 0:01.46
--   832040
--   Execution time: 0.24s / elapsed: 0:00.28
-- Curry2Go:
--   Compilation time: 1.90s / elapsed: 0:01.44
--   Execution time: 103.67s / elapsed: 0:24.43
--
main3 = fib3 30

-- > quite fast
-- > fibonacci> length $ take 100 $ fib_inf 
-- 100
-- Execution time: 3 msec. / elapsed: 6 msec.
-- PAKCS:Execution time: 2 msec. / elapsed: 6 msec.
-- KICS2:
--   KiCS2 compilation time: 1.13s / elapsed: 0:01.42
--   GHC compilation time: 1.14s / elapsed: 0:01.45
--   [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]
--   Execution time: 0.00s / elapsed: 0:00.01
-- Curry2Go:
-- Compilation time: 1.78s / elapsed: 0:01.36
-- [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181]
-- Execution time: 0.06s / elapsed: 0:00.01
-- 
main4 = take 20 $ fib_inf


--
-- PAKCS: Execution time: 17476 msec. / elapsed: 20411 msec.
-- KICS2:
--   KiCS2 compilation time: 1.69s / elapsed: 0:02.00
--   GHC compilation time: 2.01s / elapsed: 0:02.41
--   Execution time: 0.41s / elapsed: 0:00.44
-- Curry2Go:
--   Compilation time: 2.03s / elapsed: 0:01.45
--   Execution time: 165.02s / elapsed: 0:36.29
--
main5 = fibmemo 30


-- PAKCS: Execution time: 38180 msec. / elapsed: 43177 msec.
-- KICS2: KiCS2 compilation time: 1.58s / elapsed: 0:01.90 GHC compilation time: 2.42s / elapsed: 0:02.88 Execution time: 0.75s / elapsed: 0:00.78
-- Curry2Go: 
main6 = lazyMemoFibs 30