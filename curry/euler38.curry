{- 
  
  Euler #38 in Curry

  """
  Take the number 192 and multiply it by each of 1, 2, and 3:

      192 × 1 = 192
      192 × 2 = 384
      192 × 3 = 576

  By concatenating each product we get the 1 to 9 pandigital, 
  192384576. We will call 192384576 the concatenated product of 192 
  and (1,2,3)

  The same can be achieved by starting with 9 and multiplying by 
  1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the 
  concatenated product of 9 and (1,2,3,4,5).

  What is the largest 1 to 9 pandigital 9-digit number that can be 
  formed as the concatenated product of an integer with 
  (1,2, ... , n) where n > 1?
  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
-- import CLP.FD

pandigital_1_9 :: [Char] -> Bool
pandigital_1_9 xs = length xs == 9 && xs == nub xs && not (elem '0' xs)

euler38a' :: Int -> (Bool,[Char])
euler38a' n = if pandigital_1_9 t then (True,t) else (False,t)
         where
         t = last . takeWhile ((< 10) . length) . scanl1 (++) $ [show (n*i) | i <- [1..]]

euler38a :: [Char]
euler38a = head [t | n <- [9876,9875..9], (b,t) <- [euler38a' n], b == True]


main :: IO ()
main = do
         -- PACKCS: Execution time: 80 msec. / elapsed: 89 msec.
         -- KICS2: KiCS2 compilation time: 1.09s / elapsed: 0:01.31 GHC compilation time: 1.07s / elapsed: 0:01.41 Execution time: 0.00s / elapsed: 0:00.02 
         -- Curry2Go: Compilation time: 2.28s / elapsed: 0:08.26 Execution time: 0.25s / elapsed: 0:00.18
         print euler38a 
