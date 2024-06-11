{- 
  
  Euler #49 in Haskell

  """  
  The arithmetic sequence, 1487, 4817, 8147, in which each of the terms 
  increases by 3330, is unusual in two ways: (i) each of the three terms are 
  prime, and, (ii) each of the 4-digit numbers are permutations of one another.

  There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, 
  exhibiting this property, but there is one other 4-digit increasing sequence.

  What 12-digit number do you form by concatenating the three terms 
  in this sequence?
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
import HakankUtils (isPrime,numToDigits,digitsToNum,increasing)

primePerms :: Show a => a -> [[Int]]
primePerms p = filter (\d -> isPrime (digitsToNum d)) $ permutations (numToDigits p)

check_a :: (Num a, Ord a) => [[a]] -> [[a]]
check_a ps = nub [[an,bn,cn] | a <- ps, b <- ps, c <- ps, a < b, b < c,
                        an <- [digitsToNum a], bn <- [digitsToNum b],
                        (bn-an) == 3330,
                        cn <- [digitsToNum c],
                        (cn-bn) == 3330
                        ]
euler49a :: [Char]
euler49a = (concat . concat) [map show (concat t) | n <- [1001,1003..9999], n /= 1487,
                                                    isPrime n,
                                                    increasing (numToDigits n),
                                                    pp <- [primePerms n], length pp >= 3,
                                                    t <- [check_a pp], not $ null t]

main :: IO ()
main = do
         print euler49a -- (0.11 secs, 82,623,648 bytes)
