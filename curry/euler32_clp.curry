{- 
  
  Euler #32 in Curry

  Problem 32
  """
  We shall say that an n-digit number is pandigital if it makes use of 
  all the digits 1 to n exactly once; for example, the 5-digit number, 
  15234, is 1 through 5 pandigital.

  The product 7254 is unusual, as the identity, 39 × 186 = 7254, 
  containing multiplicand, multiplier, and product is 1 through 9 
  pandigital.

  Find the sum of all products whose multiplicand/multiplier/product 
  identity can be written as a 1 through 9 pandigital.
  HINT: Some products can be obtained in more than one way so be sure 
  to only include it once in your sum.
  """

  This is a CLP.FD version which only works with PAKCS.

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Control.AllValues
-- import Control.SetFunctions
import HakankUtils
import HakankUtilsCLPFD
import CLP.FD


--
-- This is a port of my Picat CP version, see euler32.pi pandigital/1.
--
euler32_clp' :: [Int]
euler32_clp' = let
             base = 10
             -- We cannot use the lengths as decision variables.
             -- But non-determinism works as well, but we then have to use Control.AllValues.allValues
             -- for collecting the results.
             len1 = 1 ? 2
             len2 = 3 ? 4
             len3 = 4
             x1 = take len1 (domain 1 9)
             x2 = take len2 (domain 1 9)
             x3 = take len3 (domain 1 9)
             x = concat [x1,x2,x3]
             num1 = head (domain 1 99)
             num2 = head (domain 100 9999)
             num3 = head (domain 1000 9999)
           in
             if len1 + len2 + len3 == 9 then
               -- If only [num3] then there's a lot of wrong solutions...
               solveFD[FirstFail] [num3,num1,num2] $               
               CLP.FD.allDifferent x /\
               num3 =# num1 * num2 /\      
               num1 =# toNum x1 base /\
               num2 =# toNum x2 base /\               
               num3 =# toNum x3 base
             else
               []

euler32_clp_a :: Int
euler32_clp_a = Data.List.sum $ nub $ map (\x -> if null x then 0 else head x ) $ allValues $ euler32_clp'


-- Using safeHead: about same time as euler_32_clp_a
euler32_clp_b :: Int
euler32_clp_b = Data.List.sum $ nub $ map (safeHead 0) $ allValues $ euler32_clp'

main :: IO ()
main = do
         -- PAKCS: Execution time: 99 msec. / elapsed: 98 msec.
         -- KICS2: Does not support CLP.FD
         -- Curry2Go: Does not support CLP.FD         
         -- print euler32_clp_a


         -- PAKCS: Execution time: 95 msec. / elapsed: 99 msec.
         -- KICS2: Does not support CLP.FD
         -- Curry2Go: Does not support CLP.FD         
         print euler32_clp_b