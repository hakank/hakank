{- 
  
  Euler #50 in Haskell

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
  Also see my Haskell page: http://hakank.org/haskell/
    
-}

import Data.List
import HakankUtils (isPrime,primesN,range)

    
euler50a = head [pp | len <- [550,549..21], offset <- [1..549],
                 -- pp <- [sum [ps !! j | j <- [(offset+1)..(offset+len)]]],
                  pp <- [sum $ range ps (offset+1) (offset+len)], -- much faster
                 pp < 1000000,
                 isPrime pp]
           where
             ps = primesN 1000000

main :: IO ()
main = do
         print euler50a -- (0.11 secs, 261,379,200 bytes)