{-

 Euler #38 in Haskell

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
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
-- import HakankUtils

euler38a' :: Int -> (Bool,[Char])
euler38a' n = if t == nub t && not (elem '0' t) then (True,t) else (False,t)
         where
         t = last . takeWhile ((< 10) . length) . scanl1 (++) $ [show (n*i) | i <- [1..]]

euler38a :: [Char]
euler38a = head [t | n <- [9876,9875..9], (b,t) <- [euler38a' n], b == True]

-- NOT CORRECT!
euler38b' n i lst
   | length lst >= 9 = lst
   | otherwise       = (show (n*i)) : euler38b' n (i+1) lst
    
euler38b = head [t | n <- [9876,9875..9], t <- [euler38b' n 1 []] ]

main :: IO ()
main = do
         print euler38a -- (0.04 secs, 2,445,984 bytes)
