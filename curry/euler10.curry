{- 
  
  Euler #10 in Curry

  Problem 10
  """ 
  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
  
  Find the sum of all the primes below two million.
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (primesN,primes,isPrime)
-- import CLP.FD


-- Very, very slow: about 14minutes in PAKCS. Much faster in KiCS2 (about 15s)

euler10a :: Int
euler10a = sum $ primesN 2000000

-- I stopped this after 20minutes
euler10b :: Int
euler10b = sum (takeWhile (< 2000000) primes)

-- PAKCS: Even slower, about 14 minutes
-- Note: This is (potentially) non-det.
euler10c' :: Int -> Int -> Int
euler10c' 0 a = a
euler10c' n a | n > 0 = if isPrime n then
                          euler10c' (n-1) (a+n)
                        else
                          euler10c' (n-1) a


euler10c :: Int
euler10c = euler10c' 2000000 0

--
-- As euler10c but deterministic. About the same time as euler10a.
--
euler10d' :: Int -> Int -> Int
euler10d' n a
         | n == 0    = a
         | otherwise = if isPrime n then
                          euler10d' (n-1) (a+n)
                        else
                          euler10d' (n-1) a

euler10d :: Int
euler10d = euler10d' 2000000 0


main :: IO ()
main = do
         -- PAKCS: Execution time: 828937 msec. / elapsed: 935451 msec 
         -- KICS2: 1.73s+1.37s+12.87s !!
         -- Curry2Go: Compilation time: 2.04s / elapsed: 0:01.55 Stopped executation after 15 minutes
         print euler10a
         
         -- PAKCS: ??? > 20 minutes 
         -- KICS2: 1.72s+1.37s+??? Stopped after 10minutes!
         -- Curry2Go: Compilation time: 1.79s / elapsed: 0:01.37 Stopped after 20 minutes
         -- print euler10b -- 
         
         -- PAKCS: Execution time: 836624 msec. / elapsed: 945886 msec. 
         -- KICS2: 1.71ss1.34s+17.81s!!!
         -- Curry2Go: Compilation time: 1.76s / elapsed: 0:01.37 ??? Stopped after 50 minutes
         -- print euler10c

         -- PAKCS: 
         -- KICS2: KiCS2 compilation time: 1.68s / elapsed: 0:02.09 GHC compilation time: 1.23s / elapsed: 0:01.69 Execution time: 12.71s / elapsed: 0:12.99

         -- Curry2Go:
         -- print euler10d