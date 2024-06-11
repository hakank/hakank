{- 
  
  Euler #50 in Curry

  """
  The prime 41, can be written as the sum of six consecutive primes:
  41 = 2 + 3 + 5 + 7 + 11 + 13

  This is the longest sum of consecutive primes that adds to a prime 
  below one-hundred.

  The longest sum of consecutive primes below one-thousand that adds to a prime, 
  contains 21 terms, and is equal to 953.
  
  Which prime, below one-million, can be written as the sum of the most 
  consecutive primes?
  """


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Debug.Trace
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (isPrime,primesN,range)
-- import CLP.FD

euler50a :: Int
euler50a =  head [pp | len <- [550,549..21], offset <- [1..549],
                 -- pp <- [sum [ps !! j | j <- [offset+1..offset+len]]],                 
                 let pp = sum $ range ps (offset+1) (offset+len), -- much faster
                 pp < 1000000,
                 isPrime pp]
            where
              ps = primesN 1000000

main :: IO ()
main = do
         -- PAKCS: Execution time: 7481 msec. / elapsed: 8332 msec.
         -- KICS2: KiCS2 compilation time: 1.85s / elapsed: 0:02.24 GHC compilation time: 3.47s / elapsed: 0:03.99 Execution time: 0.16s / elapsed: 0:00.16
         -- Curry2Go: Compilation time: 3.57s / elapsed: 0:01.95 Execution time: 33.90s / elapsed: 0:17.20
         print euler50a
