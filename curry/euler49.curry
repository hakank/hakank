{- 
  
  Euler #49 in Curry

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
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils (isPrime,numToDigits,digitsToNum,increasing)
-- import CLP.FD

primePerms :: Show a => a -> [[Int]]
primePerms p = filter (\d -> isPrime (digitsToNum d)) $ permutations (numToDigits p)

check_a :: (Num a, Ord a) => [[a]] -> [[a]]
check_a ps = nub [[an,bn,cn] | a <- ps, b <- ps, c <- ps,
                               a < b, b < c,
                               let an = digitsToNum a, let bn = digitsToNum b,
                               (bn-an) == 3330,
                               let cn = digitsToNum c,
                               (cn-bn) == 3330]

euler49a :: [Char]
euler49a = (concat . concat) [map show (concat t) | n <- [1001,1003..9999], n /= 1487, isPrime n,
                                                    increasing (numToDigits n),
                                                    let pp = primePerms n,
                                                    length pp >= 3,
                                                    let t = check_a pp,
                                                    not $ null t]

main :: IO ()
main = do
         -- PAKCS: Execution time: 3285 msec. / elapsed: 3749 msec.
         -- KICS2: KiCS2 compilation time: 1.68s / elapsed: 0:02.05 GHC compilation time: 1.61s / elapsed: 0:02.10 Execution time: 0.09s / elapsed: 0:00.10
         -- Cury2Go: Compilation time: 2.26s / elapsed: 0:01.62 Execution time: 11.30s / elapsed: 0:06.88
         print euler49a