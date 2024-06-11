{-
  Fibonacci (and memoization)
  This is a port of my Curry program fibonacci.curry


-}

import Data.Function
import Data.List
-- import HakankUtils

-- Very slow.
fib1 0 = 0
fib1 1 = 1
fib1 n | n > 1 = fib1 (n-1) + fib1 (n-2)

-- From https://en.wikibooks.org/wiki/Haskell/Fix_and_recursion
--- fib2 nn = fix $ fib1 nn
fib2 = fix (\rec n -> if n == 0 then 0 else if n == 1 then 1 else rec (n-1) + rec (n-2))

-- A little faster than fib2, but still slow
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




-- very slow
main1 = fib1 10


-- main2 = fibmemo 10
main2 = fib 109

main3 = fib3 30

-- > quite fast
-- > fibonacci> length $ take 100 $ fib_inf 
-- 100
-- Execution time: 3 msec. / elapsed: 6 msec.
main4 = take 20 $ fib_inf
        